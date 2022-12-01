
#[cfg(test)]
mod tests {

    const EXAMPLE: &str = "
    1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
    ";

    fn calculate_elf_payload(lines: &str) -> Option<i32> {
        return lines
            .split("\n")
            .map(|bite| bite.trim())
            .filter(|bite| !bite.is_empty())
            .map(|bite| bite.parse::<i32>().unwrap())
            .reduce(|acc, item| acc + item)
    }

    fn calculate_elf_payloads(puzzle: &str) -> Vec<i32> {
        return puzzle
            .split("\n\n")
            .map(|elf| calculate_elf_payload(elf))
            .filter(|elf| elf.is_some())
            .map(|elf| elf.unwrap())
            .collect::<Vec<i32>>()
    }

    fn find_max_elf_payload(puzzle: &str) -> i32 {
        *calculate_elf_payloads(puzzle)
            .iter()
            .max()
            .unwrap()
    }

    fn find_max_3_elf_payload(puzzle: &str) -> Option<i32> {
        let mut elf_payloads = calculate_elf_payloads(puzzle);
        elf_payloads.sort_by(|a, b| b.partial_cmp(a).unwrap());
        elf_payloads.iter()
            .take(3)
            .map(|x| *x)
            .reduce(|acc, item| acc + item)
    }

    #[test]
    fn day1_part1_example() {
        let actual = find_max_elf_payload(EXAMPLE);

        assert_eq!(actual, 24000);
    }

    #[test]
    fn day1_part1_puzzle() {
        let puzzle = std::fs::read_to_string("/puzzles/Day1.txt").unwrap();
        let actual = find_max_elf_payload(&puzzle);

        assert_eq!(actual, 68775);
    }

    #[test]
    fn day1_part2_example() {
        let actual = find_max_3_elf_payload(EXAMPLE);

        assert_eq!(actual, Some(45000))
    }

    #[test]
    fn day1_part2_puzzle() {
        let puzzle = std::fs::read_to_string("/puzzles/Day1.txt").unwrap();
        let actual = find_max_3_elf_payload(&puzzle);

        assert_eq!(actual, Some(202585))
    }
}
