%% -*- mode: nitrogen -*-
-module (emailform).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-compile(export_all).

main() ->
    case wf:user() /= undefined of
        true -> main_authorized();
        false -> wf:redirect_to_login("/login")
    end
.

main_authorized() -> #template { file="./site/templates/bare.html" }.

title() -> "Schedule email".

init_session() ->
    Msg = case wf:session(msg) of
        undefined ->
            esmtp_mime:msg()
        ;
        M -> M
    end,
    wf:session(msg, Msg),

    % Update the list of To, Cc and Bcc
    lists:foreach(
        fun(Address) ->
            add_to_panel(toPlaceholder, [
                #button{
                    id=delToBtn, text="-", postback={delToBtnClick, Address}
                },
                Address
            ])
        end,
        esmtp_mime:to(Msg)
    ),
    lists:foreach(
        fun(Address) ->
            add_to_panel(ccPlaceholder, [
                #button{
                    id=delCcBtn, text="-", postback={delCcBtnClick, Address}
                },
                Address
            ])
        end,
        esmtp_mime:cc(Msg)
    ),
    lists:foreach(
        fun(Address) ->
            add_to_panel(bccPlaceholder, [
                #button{
                    id=delBccBtn, text="-", postback={delBccBtnClick, Address}
                },
                Address
            ])
        end,
        esmtp_mime:bcc(Msg)
    ),

    % TODO: delete this line - used for debbug
    wf:set(sessionState, io_lib:format("~p", [Msg]))
.

body() ->
    Body = [
        #panel { style="margin: 50px 100px;", body=[
            #h2 { text="Schedule email" },

            #span{ text="State: " }, #span{ id=sessionState },

            #label{ text="to:" },
            #textbox{ id=toTextBox },
            #button{ id=addToBtn, text="+", postback=addToBtnClick },
            #panel { id=toPlaceholder },

            #label{ text="cc:" },
            #textbox{ id=ccTextBox },
            #button{ id=addCcBtn, text="+", postback=addCcBtnClick },
            #panel { id=ccPlaceholder },

            #label{ text="bcc:" },
            #textbox{ id=bccTextBox },
            #button{ id=addBccBtn, text="+", postback=addBccBtnClick },
            #panel { id=bccPlaceholder },

            #hr{},
            #label{ text="subject:" },
            #textbox{ id=subjectTextBox },
            #label{ text="body:" },
            #textarea{ id=bodyTextBox },
            #hr{},
            #label{ text="send date (yy-mm-dd):" },
            #datepicker_textbox{ id=dueDateTextBox, options=[
                {dateFormat, "yy-mm-dd"},
                {gotoCurrent, true},
                {minDate, "+0"},
                {showAnim, ""},
                {showOn, "button"}
            ]},
            % TODO: check if the time field has a valid format
            #label{ text="time (hh:mm):" },
            #textbox{ id=dueTimeTextBox },
            #table { style="width: 100%;", rows=[
                #tablerow { cells=[
                    #tableheader { style="width: 33%;", text="attachments" }
                ]},
                #tablerow { cells=[
                    #tablecell { id=attachmentsBottomCell }
                ]}
            ]},
            #upload { tag=attachmentBtnClick, show_button=false },

            #br{},
            #button{ id=submitBtn, text="Send e-mail", postback=submitBtnClick },

            #p{},
            #panel { id=placeholder }
        ]}
    ],
    wf:wire(addToBtn, toTextBox, #validate { validators=[
        #is_required { text="Required." },
        #is_email { text="Must be a valid email address."}
    ]}),
    wf:wire(addCcBtn, ccTextBox, #validate { validators=[
        #is_required { text="Required." },
        #is_email { text="Must be a valid email address."}
    ]}),
    wf:wire(addBccBtn, bccTextBox, #validate { validators=[
        #is_required { text="Required." },
        #is_email { text="Must be a valid email address."}
    ]}),
    wf:wire(submitBtn, dueTimeTextBox, #validate { validators=[
        #is_required { text="Required." },
        #custom { text="Must be a valid time (ex: 07:35).", tag=dueTimeTextBox, function=fun time_validator/2 }
    ]}),
    init_session(),
    Body
.

% TODO: add a date validator
time_validator(_, TimeStr) ->
    io:format("WTF:~p~n", [TimeStr]),
    case re:run(TimeStr, "^\\d\\d:\\d\\d$") of
        nomatch -> false;
        {match,[{0,5}]} ->
            [Hour, Minute] = string:tokens(TimeStr, ":"),
            case {list_to_integer(Hour), list_to_integer(Minute)} of
                {H, M} when H < 24, H >= 0, M < 60, M >= 0 -> true;
                _ -> false
            end
    end
.

datetime_from_str(Date, Time) ->
    {match, [Year, Month, Day]} = re:run(Date, "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)", [{capture, all_but_first, list}]),
    {match, [Hour, Minute]} = re:run(Time, "(\\d\\d):(\\d\\d)", [{capture, all_but_first, list}]),
    NDate = {list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)},
    NTime = {list_to_integer(Hour), list_to_integer(Minute), 0},
    {NDate, NTime}
.

start_upload_event(attachmentBtnClick) ->
    wf:flash("I'm here!!"),
    ok
.

finish_upload_event(_Tag, undefined, _, _) ->
    wf:flash("Please select a file."),
    ok
;
finish_upload_event(_Tag, FileName, LocalFileData, _Node) ->
    FileSize = filelib:file_size(LocalFileData),
    {ok, Binary} = file:read_file(LocalFileData),
    file:delete(LocalFileData),
    wf:insert_bottom(attachmentsBottomCell, #panel{
        % TODO: add a delete attachment
        % TODO: add line with sum of attachments size
        body=[
            #button{ id=submitBtn, text="delete", postback=submitBtnClick },
            #span{text=wf:f("~s (~p bytes)", [FileName, FileSize])}
        ],
        actions=#show{ effect=pulsate, options=[{times, 1}] }
    }),
    NMsg = esmtp_mime:add_attachment_part(wf:session(msg), FileName, Binary),
    wf:session(msg, NMsg),
    % TODO: store the LocalFileData somewhere to send with the email
    ok
.

add_to_panel(Panel, ElementList) ->
    wf:insert_top(
        Panel,
        #panel{
            body=ElementList,
            actions=#show{ effect=pulsate, options=[{times, 1}] }
        }
    )
.

event(addToBtnClick) ->
    Address = wf:q(toTextBox),
    Msg = wf:session(msg),
    AddressList = esmtp_mime:to(Msg),
    NMsg = esmtp_mime:to(Msg, [Address|AddressList]),
    wf:session(msg, NMsg),
    add_to_panel(toPlaceholder, [
        #button{
            id=delToBtn,
            text="-",
            postback={delToBtnClick, Address}
        },
        Address
    ])
;
event(addCcBtnClick) ->
    Address = wf:q(ccTextBox),
    Msg = wf:session(msg),
    AddressList = esmtp_mime:cc(Msg),
    NMsg = esmtp_mime:cc(Msg, [Address|AddressList]),
    wf:session(msg, NMsg),
    wf:insert_top(
        ccPlaceholder,
        #panel{
            body=[
                #button{
                    id=delCcBtn,
                    text="-",
                    postback={delCcBtnClick, Address}
                },
                Address
            ],
            actions=#show{ effect=pulsate, options=[{times, 1}] }
        }
    )
;
event(addBccBtnClick) ->
    Address = wf:q(bccTextBox),
    Msg = wf:session(msg),
    AddressList = esmtp_mime:bcc(Msg),
    NMsg = esmtp_mime:bcc(Msg, [Address|AddressList]),
    wf:session(msg, NMsg),
    wf:insert_top(
        bccPlaceholder,
        #panel{
            body=[
                #button{
                    id=delBccBtn,
                    text="-",
                    postback={delBccBtnClick, Address}
                },
                Address
            ],
            actions=#show{ effect=pulsate, options=[{times, 1}] }
        }
    )
;
event(submitBtnClick) ->
    % TODO: this shuld just submit the message stored in the session
    To = wf:q(toTextBox),
    Subject = wf:q(subjectTextBox),
    Body = wf:q(bodyTextBox),
    Date = wf:q(dueDateTextBox),
    Time = wf:q(dueTimeTextBox),
    DateTime = datetime_from_str(Date, Time),
    [User] = storage:get_user(wf:user()),
    SmtpConn = esmtp:conn(
        "erlymail.com",
        {User#users.smtp_srv, list_to_integer(User#users.smtp_prt)},
        User#users.smtp_ssl,
        {User#users.smtp_usr, User#users.smtp_pwd}
    ),
    Msg = wf:session(msg),
    NMsg1 = esmtp_mime:from(Msg, User#users.email),
    NMsg2 = esmtp_mime:to(NMsg1, [{To, To}]),
    NMsg3 = esmtp_mime:subject(NMsg2, Subject),
    NMsg4 = esmtp_mime:add_text_part(NMsg3, Body),
    erlymail_scheduler_srv:add(DateTime, SmtpConn, NMsg4),
    wf:insert_top(placeholder, wf:f("User info: <br />~p", [NMsg4]))
.
