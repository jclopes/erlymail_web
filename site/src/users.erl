-module(users).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-compile(export_all).

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Erly-EMAIL User list".

get_data() ->
    [[User, {viewUser, User}] || User <- mnesia:dirty_all_keys(users)]
.

get_map() ->
    [
        userName@text,
        viewBtn@postback
    ]
.

body() ->
    Data = get_data(),
    Map = get_map(),
    Body = [
        #panel { style="margin: 50px 100px;", body=[
            #span{
                text="All users:"
            },

            #bind{ id=userBinding, data=Data, map=Map, body=[
                #hr{},
                #label{ id=userName },
                #button{ id=viewBtn, text="more..." }
            ]},

            #p{},
            #panel { id=placeholder }
        ]}
    ],
    Body
.

event({viewUser, User}) ->
    UserData = storage:get_user(User),
    Res = io_lib:format(
        "<p>Data for user: ~s</p>~p<br />",
        [User, UserData]
    ),
    wf:insert_top(placeholder, Res)
.
