-module(oko_keys).
-export([generate_key_pair/0, public_key_to_address/1]).

%% Generates a public and private key pair using secp256k1.
generate_key_pair() ->
    {ok, _Curve} = crypto:ec_curves(),
    {PublicKey, PrivateKey} = crypto:ec_gen_key('secp256k1'),
    {PublicKey, PrivateKey}.

%% Converts a public key to a node address.
%% This can be used as a node's unique identifier.
public_key_to_address(PublicKey) ->
    %% We hash the public key to generate an address.
    %% This is a common approach in blockchain systems.
    Digest = crypto:hash(sha256, PublicKey),
    base64:encode(Digest).