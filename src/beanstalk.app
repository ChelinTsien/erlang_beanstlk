%% -*- mode: erlang; indent-tabs-mode: nil -*-
{application,
 beanstalk,
 [{description, "beanstalk - an Erlang implementation of beanstalk client, like php-beanstalk"},
  {vsn, "1.0"},
  {registered, [beanstalk_server]},
  {applications, [kernel, stdlib]},
  {mod, {beanstalk_app, []}},
  {env, []}
 ]}.
