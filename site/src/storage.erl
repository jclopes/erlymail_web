-module(storage).

-include("records.hrl").

-export([
    add_user/7,
    get_user/1,
    delete_user/1,
    create_schema/0
]).

create_schema() ->
    case proplists:lookup(mnesia, application:which_applications()) of
        none ->
            mnesia:create_schema([node()]),
            application:start(mnesia),
            mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}]),
            application:stop(mnesia)
        ;
        _ ->
            application:stop(mnesia),
            mnesia:create_schema([node()]),
            application:start(mnesia),
            mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}])
    end
.

add_user(Email, Password, SmtpSrv, SmtpPrt, SmtpUsr, SmtpPwd, SmtpSSL) ->
    UserRow = #users{
        email=Email,
        password=Password,
        smtp_srv=SmtpSrv,
        smtp_prt=SmtpPrt,
        smtp_usr=SmtpUsr,
        smtp_pwd=SmtpPwd,
        smtp_ssl=SmtpSSL
    },
    F = fun() ->
        mnesia:write(UserRow)
    end,
    mnesia:transaction(F)
.

delete_user(UserEmail) ->
    F = fun() ->
        mnesia:delete({users, UserEmail})
    end,
    mnesia:transaction(F)
.

get_user(UserEmail) ->
    mnesia:dirty_read({users, UserEmail})
.
