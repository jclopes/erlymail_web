%% -*- mode: nitrogen -*-
-module (emailform).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello from emailform.erl!".

body() ->
    Body = [
        #panel { style="margin: 50px 100px;", body=[
            #span { text="Hello from emailform.erl!" },

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
            #label{ text="attachments:" },
            % TODO: upload multiple files and show a list
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
            #br{},
            #button{ id=submitBtn, text="Submit", postback=submitBtnClick },

            #p{},
            #panel { id=placeholder }
        ]}
    ],
    wf:wire(submitBtn, toTextBox, #validate { validators=[
        #is_required { text="Required." }
    ]}),
    Body.

event(submitBtnClick) ->
    wf:insert_top(placeholder, "<p>You clicked the button!").
