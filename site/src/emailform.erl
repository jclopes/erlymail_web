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
                #span{ id=toAddress, body=[
                    #link{
                        text="[del]", postback={delToBtnClick, toAddress, Address}
                    },
                    #span{ html_encode=true, text=Address }
                ]}
            ])
        end,
        esmtp_mime:to(Msg)
    ),
    lists:foreach(
        fun(Address) ->
            add_to_panel(ccPlaceholder, [
                #span{ id=ccAddress, body=[
                    #link{
                        text="[del]", postback={delCcBtnClick, ccAddress, Address}
                    },
                    #span{ html_encode=true, text=Address }
                ]}
            ])
        end,
        esmtp_mime:cc(Msg)
    ),
    lists:foreach(
        fun(Address) ->
            add_to_panel(bccPlaceholder, [
                #span{ id=bccAddress, body=[
                    #link{
                        text="[del]", postback={delBccBtnClick, bccAddress, Address}
                    },
                    #span{ html_encode=true, text=Address }
                ]}
            ])
        end,
        esmtp_mime:bcc(Msg)
    ),
    lists:foreach(
        fun({FileName, Data}) ->
            FileSize = length(Data),
            add_to_panel(attachmentsBottomCell, [
                #span{ id=attchFile, body=[
                    #link{
                        text="[del]", postback={delAttachmentBtnClick, attchFile, FileName}
                    },
                    #span{ html_encode=true, text=wf:f("~s (~p bytes)", [FileName, FileSize]) }
                ]}
            ])
        end,
        esmtp_mime:attachments(Msg)
    ),

    case wf:session(submitedMsg) of
        undefined -> undefined;
        Text when is_list(Text) ->
            wf:flash(Text),
            wf:session(submitedMsg, undefined)
    end,

    % TODO: delete this line - used for debbug
    wf:replace(sessionState, #span{ id=sessionState, html_encode=true, text=io_lib:format("~p", [Msg]) })
.

body() ->
    Body = [
        #flash{},
        #panel { style="margin: 50px 100px;",   body=[
            #h2 { text="Schedule email" },

%            #span{ text="State: ", body=[ #span{ id=sessionState } ]},

            #hr{},
            #label{ text="to:" },
            #inplace_textbox{ id=toInplaceTextBox, tag=toInplaceTextBox, text="add address",
                validators=[
                    #is_required { text="Required." },
                    #is_email { text="Must be a valid email address."}
                ]
            },
            #panel { id=toPlaceholder },
            #hr{},

            #label{ text="cc:" },
            #inplace_textbox{ id=ccInplaceTextBox, tag=ccInplaceTextBox, text="add address",
                validators=[
                    #is_required { text="Required." },
                    #is_email { text="Must be a valid email address."}
                ]
            },
            #panel { id=ccPlaceholder },
            #hr{},

            #label{ text="bcc:" },
            #inplace_textbox{ id=bccInplaceTextBox, tag=bccInplaceTextBox, text="add address",
                validators=[
                    #is_required { text="Required." },
                    #is_email { text="Must be a valid email address."}
                ]
            },
            #panel { id=bccPlaceholder },

            #hr{},
            #inplace_textbox{ id=subjectTextBox, tag=subjectInplaceTextBox, text="add subject" },
            #textarea{ id=bodyTextBox, placeholder="body" },
            #hr{},
            #label{ text="deliver day and hour:"},
            % TODO: add timezone selector
            #datepicker_textbox{ id=dueDateTextBox, style="width: 140px;", text="set date (yy-mm-dd)", options=[
                {dateFormat, "yy-mm-dd"},
                {gotoCurrent, true},
                {minDate, "+0"},
                {showAnim, ""},
                {showOn, "focus"}
            ]},
            % TODO: check if the time field has a valid format
            #inplace_textbox{ id=dueTimeTextBox, tag=dueTimeTextBoxTag, text="set time (hh:mm)",
                validators=[
                    #is_required { text="Required." },
                    #custom { text="Must be a valid time (ex: 07:35).", tag=dueTimeTextBoxTag, function=fun time_validator/2 }
                ]
            },
            #label{ text="timezone:"},
            #dropdown { id=timezoneDropDown, value="+1", options=[
                #option { text="UTC-12", value="-12" },
                #option { text="UTC-11", value="-11" },
                #option { text="UTC-10", value="-10" },
                #option { text="UTC-9", value="-9" },
                #option { text="UTC-8", value="-8" },
                #option { text="UTC-7", value="-7" },
                #option { text="UTC-6", value="-6" },
                #option { text="UTC-5", value="-5" },
                #option { text="UTC-4", value="-4" },
                #option { text="UTC-3", value="-3" },
                #option { text="UTC-2", value="-2" },
                #option { text="UTC-1", value="-1" },
                #option { text="UTC+0", value="+0" },
                #option { text="UTC+1", value="+1" },
                #option { text="UTC+2", value="+2" },
                #option { text="UTC+3", value="+3" },
                #option { text="UTC+4", value="+4" },
                #option { text="UTC+5", value="+5" },
                #option { text="UTC+6", value="+6" },
                #option { text="UTC+7", value="+7" },
                #option { text="UTC+8", value="+8" },
                #option { text="UTC+9", value="+9" },
                #option { text="UTC+10", value="+10" },
                #option { text="UTC+11", value="+11" },
                #option { text="UTC+12", value="+12" },
                #option { text="UTC+13", value="+13" },
                #option { text="UTC+14", value="+14" }
                ]},
            #hr{},

            #table { style="width: 100%;", rows=[
                #tablerow { cells=[
                    #tableheader { style="width: 33%;", text="attachments:" }
                ]},
                #tablerow { cells=[
                    #tablecell { id=attachmentsBottomCell }
                ]}
            ]},
            #upload { tag=attachmentBtnClick, show_button=false, file_text="remove this button", droppable=true, droppable_text="drop files here..." },

            #br{},
            #button{ id=submitBtn, text="Send e-mail", postback=submitBtnClick },

            #p{},
            #panel { id=placeholder, html_encode=true }
        ]}
    ],
    init_session(),
    Body
.

% TODO: add a date validator
time_validator(_, TimeStr) ->
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

datetime_from_str(Date, Time, Timezone) ->
    {match, [Year, Month, Day]} = re:run(Date, "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)", [{capture, all_but_first, list}]),
    {match, [Hour, Minute]} = re:run(Time, "(\\d\\d):(\\d\\d)", [{capture, all_but_first, list}]),
    NTimezoneSeconds = list_to_integer(Timezone) * 3600,
    NDate = {list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)},
    NTime = {list_to_integer(Hour), list_to_integer(Minute), 0},
    GS = calendar:datetime_to_gregorian_seconds({NDate, NTime}) - NTimezoneSeconds,
    calendar:gregorian_seconds_to_datetime(GS)
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
        % TODO: FileName can match multiple files, add extra info to know what to delete
        body=[
            #span{ id=attchFile, body=[
                #link{ text="[del]", postback={delAttachmentBtnClick, attchFile, FileName}},
                #span{text=wf:f("~s (~p bytes)", [FileName, FileSize])}
            ]}
        ],
        actions=#show{ effect=pulsate, options=[{times, 1}] }
    }),
    Msg = wf:session(msg),
    NMsg = esmtp_mime:add_attachment_part(Msg, FileName, Binary),
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

inplace_textbox_event(toInplaceTextBox, Address) ->
    Msg = wf:session(msg),
    AddressList = esmtp_mime:to(Msg),
    NMsg = esmtp_mime:to(Msg, [Address|AddressList]),
    EncAddress = hd(esmtp_mime:to(NMsg)),
    wf:session(msg, NMsg),
    add_to_panel(toPlaceholder, [
        #span{ id=toAddress, body=[
            #link{
                text="[del]",
                postback={delToBtnClick, toAddress, EncAddress}
            },
            #span{ html_encode=true, text=EncAddress }
        ]}
    ]),
    "add address"
;
inplace_textbox_event(ccInplaceTextBox, Address) ->
    Msg = wf:session(msg),
    AddressList = esmtp_mime:cc(Msg),
    NMsg = esmtp_mime:cc(Msg, [Address|AddressList]),
    EncAddress = hd(esmtp_mime:cc(NMsg)),
    wf:session(msg, NMsg),
    add_to_panel(ccPlaceholder, [
        #span{ id=ccAddress, body=[
            #link{
                text="[del]",
                postback={delCcBtnClick, ccAddress, EncAddress}
            },
            #span{ html_encode=true, text=EncAddress }
        ]}
    ]),
    "add address"
;
inplace_textbox_event(bccInplaceTextBox, Address) ->
    Msg = wf:session(msg),
    AddressList = esmtp_mime:bcc(Msg),
    NMsg = esmtp_mime:bcc(Msg, [Address|AddressList]),
    EncAddress = hd(esmtp_mime:bcc(NMsg)),
    wf:session(msg, NMsg),
    add_to_panel(bccPlaceholder, [
        #span{ id=bccAddress, body=[
            #link{
                text="[del]",
                postback={delBccBtnClick, bccAddress, EncAddress}
            },
            #span{ html_encode=true, text=EncAddress }
        ]}
    ]),
    "add address"
;
inplace_textbox_event(subjectInplaceTextBox, Value) ->
    Msg = wf:session(msg),
    NMsg = esmtp_mime:subject(Msg, Value),
    wf:session(msg, NMsg),
    Value
;
inplace_textbox_event(dueTimeTextBoxTag, Value) ->
    % TODO: verify that Value is a proper timestamp
    Time = Value,
    wf:session(dueTime, Time),
    io:format("dueTime: ~p~n", [Time]),
    Value
;
inplace_textbox_event(_, Value) ->
    Value
.

event({delToBtnClick, toAddress, Address}) ->
    Msg = wf:session(msg),
    AddressList = esmtp_mime:to(Msg),
    NAddressList = lists:delete(Address, AddressList),
    NMsg = esmtp_mime:to(Msg, NAddressList),
    wf:session(msg, NMsg),
    wf:remove(toAddress)
;
event({delCcBtnClick, ccAddress, Address}) ->
    Msg = wf:session(msg),
    AddressList = esmtp_mime:cc(Msg),
    NAddressList = lists:delete(Address, AddressList),
    NMsg = esmtp_mime:cc(Msg, NAddressList),
    wf:session(msg, NMsg),
    wf:remove(ccAddress)
;
event({delBccBtnClick, bccAddress, Address}) ->
    Msg = wf:session(msg),
    AddressList = esmtp_mime:bcc(Msg),
    NAddressList = lists:delete(Address, AddressList),
    NMsg = esmtp_mime:bcc(Msg, NAddressList),
    wf:session(msg, NMsg),
    wf:remove(bccAddress)
;
event({delAttachmentBtnClick, attchFile, FileName}) ->
    Msg = wf:session(msg),
    NMsg = esmtp_mime:del_attachment_part(Msg, FileName),
    wf:session(msg, NMsg),
    wf:remove(attchFile)
;
event(submitBtnClick) ->
    % TODO: this shuld just submit the message stored in the session
    BodyText = wf:q(bodyTextBox),
    Date = wf:q(dueDateTextBox),
    Time = wf:session(dueTime),
    Timezone = wf:q(timezoneDropDown),
    DateTime = datetime_from_str(Date, Time, Timezone),
    [User] = storage:get_user(wf:user()),
    SmtpConn = esmtp:conn(
        "erlymail.com",
        {User#users.smtp_srv, list_to_integer(User#users.smtp_prt)},
        User#users.smtp_ssl,
        {User#users.smtp_usr, User#users.smtp_pwd}
    ),
    Msg = wf:session(msg),
    NMsg1 = esmtp_mime:add_text_part(Msg, BodyText),
    NMsg2 = esmtp_mime:from(NMsg1, User#users.email),
    erlymail_scheduler_srv:add(DateTime, SmtpConn, NMsg2),
    reset_form("message submited"),
    wf:redirect("/emailform")
.

reset_form(SubmitedMsg) ->
    wf:session(msg, esmtp_mime:msg()),
    wf:session(dueTime, undefined),
    wf:session(submitedMsg, SubmitedMsg)
.
