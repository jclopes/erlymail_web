%% -*- mode: nitrogen -*-
-module (emailform).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-compile(export_all).

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Schedule email".

body() ->
    Body = [
        #panel { style="margin: 50px 100px;", body=[
            #h2 { text="Schedule email" },

            #p{},
            #label{ text="to:" },
            #textbox{ id=toTextBox },
            #label{ text="cc:" },
            #textbox{ id=ccTextBox },
            #label{ text="bcc:" },
            #textbox{ id=bccTextBox },
            #label{ text="subject:" },
            #textbox{ id=subjectTextBox },
            #label{ text="body:" },
            #textarea{ id=bodyTextBox },
            #label{ text="send date (yy-mm-dd):" },
            #datepicker_textbox{ id=dueDateTextBox, options=[
                {dateFormat, "yy-mm-dd"},
                {gotoCurrent, true},
                {minDate, "+0"},
                {showAnim, ""},
                {showOn, "button"}
            ]},
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
            #button{ id=submitBtn, text="Submit", postback=submitBtnClick },

            #p{},
            #panel { id=placeholder }
        ]}
    ],
    Body
.

datetime_from_str(Date, Time) ->
    {match, [Year, Month, Day]} = re:run(Date, "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)", [{capture, all_but_first, list}]),
    {match, [Hour, Minute]} = re:run(Time, "(\\d\\d):(\\d\\d)", [{capture, all_but_first, list}]),
    NDate = {list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)},
    NTime = {list_to_integer(Hour), list_to_integer(Minute)},
    {NDate, NTime}
.

start_upload_event(attachmentBtnClick) ->
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
    wf:insert_bottom(attachmentsBottomCell, #panel {
        % TODO: add a delete attachment
        % TODO: add line with sum of attachments size
        body=wf:f("~s (~p bytes)", [FileName, FileSize]),
        actions=#show { effect=pulsate, options=[{times, 1}] }
    }),
    % TODO: store the LocalFileData somewhere to send with the email
    ok
.

event(submitBtnClick) ->
    To = wf:q(toTextBox),
    Subject = wf:q(subjectTextBox),
    Body = wf:q(bodyTextBox),
    Date = wf:q(dueDateTextBox),
    Time = wf:q(dueTimeTextBox),
    DateTime = datetime_from_str(Date, Time),
    [User] = storage:get_user(wf:user()),
    SmtpConn = esmtp:conn(
        "erlymail.com",
        {User#users.smtp_srv, User#users.smtp_prt},
        {User#users.smtp_usr, User#users.smtp_pwd}
    ),
    Message = esmtp_mime:msg(User#users.email, To, Subject, Body),
    erlymail_scheduler_srv:add(DateTime, SmtpConn, Message),
    wf:insert_top(placeholder, wf:f("User info: <br />~p", [User]))
.
