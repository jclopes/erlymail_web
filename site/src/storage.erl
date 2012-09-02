-module(storage).

-export([
    add_user/6,
    get_user/1,
    delete_user/1,
    create_schema/0
]).

-record(users, {email, password, smtp_srv, smtp_prt, smtp_usr, smtp_pwd}).

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

add_user(Email, Password, SmtpSrv, SmtpPrt, SmtpUsr, SmtpPwd) ->
    UserRow = #users{
        email=Email,
        password=Password,
        smtp_srv=SmtpSrv,
        smtp_prt=SmtpPrt,
        smtp_usr=SmtpUsr,
        smtp_pwd=SmtpPwd
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
