import 'dart:io';
import 'dart:async';

Iterable<int> readInput(Iterable<String> lines) {
  return lines.map((line) => int.parse(line));
}

int moduleFuel(int mass) => ((mass ~/ 3) - 2);

int sumList (Iterable<int> list) => list.reduce((sum, el) => sum + el);

int includingFuelForFuel(int fuel) {
  int totalFuel = 0;
  int latestFuel = fuel;
  while (latestFuel > 0) {
    totalFuel += latestFuel;
    latestFuel = moduleFuel(latestFuel);
  }
  return totalFuel;
}

Future runDay1() async {
  assert(moduleFuel(12) == 2);
  assert(moduleFuel(1969) == 654);
  assert(includingFuelForFuel(moduleFuel(1969)) == 966);
  assert(includingFuelForFuel(moduleFuel(100756)) == 50346);

  var input = File('inputs/day1.txt');
  Iterable<String> contents = await input.readAsLines();
  Iterable<int> masses = readInput(contents.map((s) =>
      s.trim()).where((s) => s.isNotEmpty));
  var moduleFuels = masses.map(moduleFuel);
  var totalModuleFuels = sumList(moduleFuels);
  return [totalModuleFuels,
    sumList(moduleFuels.map(includingFuelForFuel))];
}