%% Copyright 2011 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(tpot_qrcode_reedsolomon).

-export([encode/2, bch_code/2]).

-define(QRCODE_GF256_PRIME_MODULUS, 285). % 16#011D = 2^8 + 2^4 + 2^3 + 2^2 + 2^0

-spec encode(binary(), integer()) -> binary().
encode(Bin, Degree) when Degree > 0 ->
	Field = tpot_gf256:field(?QRCODE_GF256_PRIME_MODULUS),
	Generator = generator(Field, Degree),
	Data = binary_to_list(Bin),
	Coeffs = tpot_gf256:monomial_product(Field, Data, 1, Degree),
	{_Quotient, Remainder} = tpot_gf256:divide(Field, Coeffs, Generator),
	ErrorCorrectionBytes = list_to_binary(Remainder),
	<<ErrorCorrectionBytes/binary>>.

-spec bch_code(binary(), integer()) -> binary().
bch_code(Byte, Poly) ->
	MSB = msb(Poly),
	Byte0 = Byte bsl (MSB - 1),
	bch_code(Byte0, Poly, MSB).


%% Internal

%%
generator(F, D) when D > 0 ->
	generator(F, [1], D, 0).
%	
generator(_, P, D, D) ->
	P;
generator(F, P, D, Count) ->
	P0 = tpot_gf256:polynomial_product(F, P, [1, tpot_gf256:exponent(F, Count)]),
	generator(F, P0, D, Count + 1).
	
%
bch_code(Byte, Poly, MSB) ->
	case msb(Byte) >= MSB of
	true ->
		Byte0 = Byte bxor (Poly bsl (msb(Byte) - MSB)),
		bch_code(Byte0, Poly, MSB);
	false ->
		Byte
	end.

%%
msb(0) ->
	0;
msb(Byte) ->
	msb(Byte, 0).
msb(0, Count) ->
	Count;
msb(Byte, Count) ->
	msb(Byte bsr 1, Count + 1).

