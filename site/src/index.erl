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
        #label{ text="BETA version" },
        #p{},
        "
        This is a test version. Use only data that can be made public and/or lost within this site.
        ",
        #p{},
        #link { text="login", url="/login" },
        #p{},
        #link { text="signup", url="/signup" }
    ].

event(_) ->
    % nothing
    undefined
.
