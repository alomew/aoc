import 'package:test/test.dart';
import 'package:dart2019/day1.dart' as day1;

void main() {
  test("Final answer for part 1", () async {
    var part1 = (await day1.runDay1())[0];
    expect(3216868, equals(part1));
  });
}