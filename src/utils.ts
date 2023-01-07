import {
  Address,
  getAddressDetails,
} from "https://deno.land/x/lucid@0.8.7/mod.ts";
import * as D from "./contract.types.ts";

export function fromAddress(address: Address): D.Address {
  // We do not support pointer addresses!

  const { paymentCredential, stakeCredential } = getAddressDetails(
    address,
  );

  if (!paymentCredential) throw new Error("Not a valid payment address.");

  return {
    paymentCredential: paymentCredential?.type === "Key"
      ? {
        PublicKeyCredential: [paymentCredential.hash],
      }
      : { ScriptCredential: [paymentCredential.hash] },
    stakeCredential: stakeCredential
      ? {
        Inline: [
          stakeCredential.type === "Key"
            ? {
              PublicKeyCredential: [stakeCredential.hash],
            }
            : { ScriptCredential: [stakeCredential.hash] },
        ],
      }
      : null,
  };
}
