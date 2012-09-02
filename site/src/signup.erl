%% -*- mode: nitrogen -*-
-module (signup).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Erly-EMAIL signup".

body() ->
    Body = [
        #panel { style="margin: 50px 100px;", body=[
            #span{
                text="Please inserte your email server access information."
            },

            #p{},
            #label{ text="email" },
            #textbox{ id=email },
            #p{},
            #label{ text="password" },
            #password{ id=password },

            #p{},
            #label{ text="smtp server" },
            #textbox{ id=smtp_srv },
            #p{},
            #label{ text="smtp server port" },
            #textbox{ id=smtp_srv_port },
            #p{},
            #label{ text="smtp user" },
            #textbox{ id=smtp_usr },
            #p{},
            #label{ text="smtp server password" },
            #password{ id=smtp_password },

            #p{},
            #button { id=signupBtn, text="signup", postback=signupBtnClick },

            #p{},
            #panel { id=placeholder }
        ]}
    ],
    wf:wire(signupBtn, email, #validate { validators=[
        #is_required { text="Required." },
        #is_email { text="Must be a valid email address."}
    ]}),
    wf:wire(signupBtn, password, #validate { validators=[
        #is_required { text="Required." }
    ]}),
    wf:wire(signupBtn, smtp_srv, #validate { validators=[
        #is_required { text="Required." }
    ]}),
    wf:wire(signupBtn, smtp_srv_port, #validate { validators=[
        #is_required { text="Required." },
        #is_integer { text="Must be a number." }
    ]}),
    wf:wire(signupBtn, smtp_usr, #validate { validators=[
        #is_required { text="Required." }
    ]}),
    wf:wire(signupBtn, smtp_password, #validate { validators=[
        #is_required { text="Required." }
    ]}),
    Body
.

event(signupBtnClick) ->
    Email = wf:q(email),
    Password = wf:q(password),
    SmtpSrv = wf:q(smtp_srv),
    SmtpSrvPrt = wf:q(smtp_srv_port),
    SmtpUsr = wf:q(smtp_usr),
    SmtpPass = wf:q(smtp_password),
    storage:add_user(Email, Password, SmtpSrv, SmtpSrvPrt, SmtpUsr, SmtpPass),
    Res = io_lib:format(
        "<p>You clicked the button!</p> ~s<br />~w<br />",
        [Email, Password]
    ),
    wf:insert_top(placeholder, Res).
