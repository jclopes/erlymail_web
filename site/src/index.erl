%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to Erly-EMAIL".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() ->
    [
        #h1 { text="Welcome to Erly-EMAIL" },
        #label{ text="TODO:" },
        #p{},
        "
        rebar bin is broken on OSX.<br />
        'nitrogen_core/validators/validator_is_email.erl' is broken regexp.
        remove the underscore conversion to sub-folders. (nitrogen_core/handlers/router)<br />
        improve the project compilation.<br />
        make it possible to have more than 1 nitrogen site on the same machine.<br />
        ",
        #p{},
        #link { text="login", url="/login" },
        #p{},
        #link { text="signup", url="/signup" }
    ].

event(click) ->
    wf:replace(button, #panel {
        body="You clicked the button!",
        actions=#effect { effect=highlight }
    }).
