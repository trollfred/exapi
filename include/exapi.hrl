-record(state, {sref = "", host = "", port = 80}).

%% Default connection settings
-define(USER, "root").
-define(PORT, 80).

%% Session
-define(LOGIN, 'session.login_with_password').
-define(LOGOUT, 'session.logout').

%% VM
-define(START, 'VM.start').
-define(CLEAN_SHUTDOWN, 'VM.clean_shutdown').
-define(CLEAN_REBOOT, 'VM.clean_reboot').
-define(VM_GET_ALL, 'VM.get_all').
-define(VM_GET_RECORD, 'VM.get_record').
-define(VM_METRICS_GET_RECORD, 'VM_metrics.get_record').
-define(GET_BY_NAME_LABEL, 'VM.get_by_name_label').
