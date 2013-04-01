
-type itag() :: bitstring().

-type imps_proto() :: keeper | chimp | iamb_pent | pan.

-record(imps_packet, {version = 1 :: integer(),
                      seq = 0 :: integer(),
                      proto :: imps_proto(),
                      src :: itag(),
                      dst :: itag(),
                      data :: binary()}).

-type imps_packet() :: #imps_packet{}.

-type imps_req_or_resp() :: keeper_req() | keeper_resp() |
    chimp_resp() | chimp_req() | iamb_pent_req() | iamb_pent_resp() |
    pan_req() | pan_resp().


-type keeper_req() :: reserved | status | heartbeat | wakeup |type |
    faster | transcript | stop | {future, integer()} | {user, integer()}.
-type keeper_resp() :: reserved | asleep | gone | distracted | noresponse | alive |
    dead | accept | refuse | {future, integer()} | {user, integer()}.

-record(keeper_pdu, {version = 1 :: integer(),
                     type :: request | response,
                     id :: integer(),
                     message :: keeper_req() | keeper_resp()}).


-type chimp_resource() :: food | water | veterinarian | technician.
-type chimp_item() :: typewriter | paper | ribbon | chair | table | monkey.
-type chimp_status() ::  asleep | gone | distracted | noresponse | alive |
    dead.

-type chimp_req() :: {send, chimp_resource()} |
                     {replace, chimp_item()} |
                     {clean, chimp_item()} |
                     {notify, chimp_status()} |
                     {transcript, Data :: binary()} |
                     bye.

-type chimp_resp() :: {helo, Text :: string()} |
                      accept | delay | refuse | received.

%% I18N: meter not enforced
-type iamb_pent_req() :: {receiveth, Name :: string()} |
                         {anon, Data :: binary()} |
                         aborteth | {aborteth, Syllables :: [string()]}.
-type iamb_pent_resp() :: hark | {hark, Syllables :: [string()]} |
                         prithee | {prithee, Syllables :: [string()]} |
                         regretteth | {regretteth, Syllables :: [string()]} |
                         accepteth | {accepteth, Syllables :: [string()]}.

-type pan_req() :: compliment | {compliment, Text :: string()} |
                   {transcript, Name :: string(), Data :: binary()} |
                   thanks.

-type pan_resp() :: sigh | {sigh, Insult :: string()} |
                   impress_me |
                   reject | {reject, Code :: integer()} | {reject, 0, Text :: binary()} |
                   dont_call_us_we_will_call_you.
