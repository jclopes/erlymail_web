%% -*- mode: nitrogen -*-
-module (login).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-compile(export_all).

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Erly-EMAIL login".

body() ->
    Body = [
        #flash {},
        #panel { style="margin: 50px 100px;", body=[
            #span { text="Please enter your email address and password.", body=[
                #label{ text="email" },
                #textbox{ id=email, next=password },
                #label{ text="password" },
                #password{ id=password, next=loginButton },
                #br{},
                #button { id=loginButton, text="login", postback=click }
            ]},
            #hr{},

            #span { body=["Or signup here: ", #link { text="signup", url="/signup" }]}

        ]}
    ],
    wf:wire(loginButton, email, #validate {
        validators=[
            #is_required { text="Required." },
            #is_email { text="Enter a valid email address." }
        ]
    }),
    wf:wire(loginButton, password, #validate {
        validators=[
            #is_required { text="Required." },
            #min_length { length=2, text="Password must be at least 2 characters long." }
        ]
    }),
    Body
.

event(click) ->
    % clear previous session
    wf:logout(),
    Email = wf:q(email),
    Password = wf:q(password),
    case authenticate(Email, Password) of
        {error, Error} ->
            wf:flash(Error),
            wf:redirect_from_login("/login")
        ;
        {ok, UserId} ->
            wf:user(UserId),
            [wf:session(K, V) || {K, V} <- load_session(UserId)],
            wf:redirect_from_login("/emailform")
    end
.

authenticate(Email, Password) ->
    % TODO: validate Email + Password and return user id
    [User] = storage:get_user(Email),
    case User#users.password of
        Password ->
            {ok, Email}
        ;
        _ ->
            {error, "Fail to authenticate"}
    end
.

load_session(UserId) ->
    % TODO: load user info from the DB
    [User] = storage:get_user(UserId),

    % Session =
    [
        { email, User#users.email },
        { smtp_srv, User#users.smtp_srv },
        { smtp_prt, User#users.smtp_prt },
        { smtp_usr, User#users.smtp_usr },
        { smtp_pwd, User#users.smtp_pwd },
        { smtp_ssl, User#users.smtp_ssl }
    ]
.
