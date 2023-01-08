import {
  Block,
  BlockShelleyCompatible,
  createClient,
  Point,
  toShelleyCompatibleBlock,
  TxShelleyCompatible,
} from "https://raw.githubusercontent.com/spacebudz/denosync/0.1.2/mod.ts";

const POLICY_ID = Deno.args[0];
const OGMIOS_URL = Deno.args[1];
const TWITTER_API_URL = Deno.args[2];
const TWITTER_API_KEY = Deno.args[3];

type WormholeEvent = { point: Point; id: number };

let events: WormholeEvent[] = [];

function rollForward(block: Block, hasExited: boolean) {
  const { blockShelley } = toShelleyCompatibleBlock(block)!;
  const point: Point = {
    hash: blockShelley.headerHash,
    slot: blockShelley.header.slot,
  };
  events = triggerEvents(point, events);

  watchBlock(blockShelley);
}

function rollBackward(point: Point) {
  events = events.filter((event) =>
    point.slot >= event.point.slot && point.hash !== event.point.hash
  );
}

function triggerEvents(
  point: Point,
  events: WormholeEvent[],
  confirmations = 3,
) {
  return events.filter((event) =>
    point.slot - event.point.slot < confirmations * 20 ? true : (() => {
      console.log("Wormhole event: SpaceBud #" + event.id);
      // TODO:
      // fetch(TWITTER_API_URL, {
      //   method: "POST",
      //   headers: {
      //     Authorization: "Bearer " + TWITTER_API_KEY,
      //     "Content-Type": "application/json",
      //   },
      //   body: JSON.stringify({
      //     id: event.id.toString(),
      //     type: "wormhole",
      //   }),
      // }).catch(console.log);
      return false;
    })()
  );
}

function watchWormhole(tx: TxShelleyCompatible, point: Point) {
  if (
    !Object.keys(tx.body.mint?.assets || {})[0]?.startsWith(POLICY_ID)
  ) return;

  const ids = Array.from(
    new Set(
      Object.keys(tx.body.mint!.assets!).flatMap((unit) => {
        const [policyId, assetName] = unit.split(".");
        if (policyId !== POLICY_ID) return [];
        return parseInt(toText(assetName.slice(14)));
      }),
    ),
  );
  ids.forEach((id) => events.push({ point, id }));
}

function tryWatch(
  tx: TxShelleyCompatible,
  point: Point,
  watcher: (tx: TxShelleyCompatible, point: Point) => unknown,
) {
  try {
    watcher(tx, point);
  } catch (e) {
    console.log(e);
  }
}

function watchBlock(blockShelley: BlockShelleyCompatible) {
  const transactions = blockShelley.body;
  const point: Point = {
    hash: blockShelley.headerHash,
    slot: blockShelley.header.slot,
  };
  for (const tx of transactions) {
    tryWatch(tx, point, watchWormhole);
  }
}

function toText(hex: string): string {
  let result = "";
  for (let i = 0; i < hex.length; i += 2) {
    result += String.fromCharCode(parseInt(hex[i] + hex[i + 1], 16));
  }
  return result;
}

const client = await createClient({
  url: OGMIOS_URL,
  startPoint: "tip",
}, {
  rollBackward,
  rollForward,
});

client.start();
console.log("Listening to wormhole");
