/*
  Two learnings:
  1. Maps are O(1) access, arrays are slower access
  2. Node does _not_ support TCO
 */

let start;

function playMap(end, count, startNumber, mem) {
  let currentNumber = startNumber;
  let nextNumber;

  while (count < end) {
    if (count % 1000000 === 0) {
      console.log(`${(Date.now() - start) / 1000}: ${count}`);
    }

    if (mem.has(currentNumber)) {
      nextNumber = count - 1 - mem.get(currentNumber);
    } else {
      nextNumber = 0;
    }

    mem.set(currentNumber, count-1);

    currentNumber = nextNumber;

    count++;
  }

  return currentNumber;
}

function play(end, count, startNumber, mem) {
  let currentNumber = startNumber;
  let nextNumber;

  while (count < end) {
    if (count % 1000000 === 0) {
      console.log(`${(Date.now() - start) / 1000}: ${count}`);
    }

    if (mem[currentNumber] !== false) {
      nextNumber = count - 1 - mem[currentNumber];
    } else {
      nextNumber = 0;
    }

    mem[currentNumber] = count-1;

    currentNumber = nextNumber;

    count++;
  }

  return currentNumber;
}

function translateValuesMap(v) {
  return new Map(v.map((val, i) => [val, i]));
}

function translateValues(v, length) {
  const result = new Array(length);
  result.fill(false);

  v.forEach((val, i) => result[val] = i);

  return result;
}

const end = 30000000;

start = Date.now();
console.log(playMap(end, 4, 0, translateValuesMap([0,3,6])));
console.log(`Total time: ${(Date.now() - start) / 1000} seconds`);

start = Date.now();
console.log(play(end, 4, 0, translateValues([0,3,6], end)));
console.log(`Total time: ${(Date.now() - start) / 1000} seconds`);
