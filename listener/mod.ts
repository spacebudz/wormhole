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

export const { eventHandler }: {
  eventHandler: (ids: number | number[]) => unknown;
} = await import("file:///" + Deno.args[1]);

type WormholeEvent = { point: Point; id: number };

let events: WormholeEvent[] = [];

async function rollForward(block: Block) {
  const { blockShelley } = toShelleyCompatibleBlock(block)!;
  const point: Point = {
    hash: blockShelley.headerHash,
    slot: blockShelley.header.slot,
  };
  events = triggerEvents(point, events);

  await watchBlock(blockShelley);
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
      eventHandler(event.id);
      return false;
    })()
  );
}

async function watchWormhole(tx: TxShelleyCompatible, point: Point) {
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
  await eventHandler(ids);
}

async function tryWatch(
  tx: TxShelleyCompatible,
  point: Point,
  watcher: (tx: TxShelleyCompatible, point: Point) => unknown,
) {
  try {
    await watcher(tx, point);
  } catch (e) {
    console.log(e);
  }
}

async function watchBlock(blockShelley: BlockShelleyCompatible) {
  const transactions = blockShelley.body;
  const point: Point = {
    hash: blockShelley.headerHash,
    slot: blockShelley.header.slot,
  };
  for (const tx of transactions) {
    await tryWatch(tx, point, watchWormhole);
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
