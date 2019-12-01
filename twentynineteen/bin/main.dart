import 'package:twentynineteen/day1.dart' as day1;

main(List<String> arguments) async {
  final List<List<String>> dayAnswers = [await day1.runDay1()];
  Iterable.generate(dayAnswers.length, (x) => x + 1).forEach((day) {
    print("Day $day\n"
          "======");
    print("Part 1: ${dayAnswers[day-1][0]}");
    print("Part 2: ${dayAnswers[day-1][1]}");
    print("\n");
  });
}
