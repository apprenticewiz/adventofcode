use std::cmp;
use std::env;
use std::fs;
use std::io;
use std::process;

#[derive(Clone, Eq, PartialEq)]
enum ItemType {
    Armor,
    Weapon,
    Ring,
}

#[derive(Clone, Eq, PartialEq)]
struct Item {
    name: String,
    kind: ItemType,
    cost: i32,
    damage: i32,
    armor: i32,
}

#[derive(Clone, Eq, PartialEq)]
struct Player {
    hit_points: i32,
    damage: i32,
    armor: i32,
}

#[derive(Clone, Eq, PartialEq)]
enum Outcome {
    Win,
    Die,
}

fn usage(progname: &str) {
    eprintln!("usage: {progname} <input file>");
    process::exit(1);
}

fn init_store() -> Vec<Item> {
    vec![
        Item {
            name: String::from("Dagger"),
            kind: ItemType::Weapon,
            cost: 8,
            damage: 4,
            armor: 0,
        },
        Item {
            name: String::from("Shortsword"),
            kind: ItemType::Weapon,
            cost: 10,
            damage: 5,
            armor: 0,
        },
        Item {
            name: String::from("Warhammer"),
            kind: ItemType::Weapon,
            cost: 25,
            damage: 6,
            armor: 0,
        },
        Item {
            name: String::from("Longsword"),
            kind: ItemType::Weapon,
            cost: 40,
            damage: 7,
            armor: 0,
        },
        Item {
            name: String::from("Greataxe"),
            kind: ItemType::Weapon,
            cost: 74,
            damage: 8,
            armor: 0,
        },
        Item {
            name: String::from("Leather"),
            kind: ItemType::Armor,
            cost: 13,
            damage: 0,
            armor: 1,
        },
        Item {
            name: String::from("Chainmail"),
            kind: ItemType::Armor,
            cost: 31,
            damage: 0,
            armor: 2,
        },
        Item {
            name: String::from("Splintmail"),
            kind: ItemType::Armor,
            cost: 53,
            damage: 0,
            armor: 3,
        },
        Item {
            name: String::from("Bandedmail"),
            kind: ItemType::Armor,
            cost: 75,
            damage: 0,
            armor: 4,
        },
        Item {
            name: String::from("Platemail"),
            kind: ItemType::Armor,
            cost: 102,
            damage: 0,
            armor: 5,
        },
        Item {
            name: String::from("Damage +1"),
            kind: ItemType::Ring,
            cost: 25,
            damage: 1,
            armor: 0,
        },
        Item {
            name: String::from("Damage +2"),
            kind: ItemType::Ring,
            cost: 50,
            damage: 2,
            armor: 0,
        },
        Item {
            name: String::from("Damage +3"),
            kind: ItemType::Ring,
            cost: 100,
            damage: 3,
            armor: 0,
        },
        Item {
            name: String::from("Defense +1"),
            kind: ItemType::Ring,
            cost: 20,
            damage: 0,
            armor: 1,
        },
        Item {
            name: String::from("Defense +2"),
            kind: ItemType::Ring,
            cost: 40,
            damage: 0,
            armor: 2,
        },
        Item {
            name: String::from("Defense +3"),
            kind: ItemType::Ring,
            cost: 80,
            damage: 0,
            armor: 3,
        },
    ]
}

fn parse_file(filename: &str) -> io::Result<Player> {
    let content = fs::read_to_string(filename)?;
    let mut line_it = content.lines();
    if let Some(first_line) = line_it.next()
        && let Some((_, hp_str)) = first_line.split_once(": ")
        && let Some(second_line) = line_it.next()
        && let Some((_, damage_str)) = second_line.split_once(": ")
        && let Some(third_line) = line_it.next()
        && let Some((_, armor_str)) = third_line.split_once(": ")
    {
        let hit_points = hp_str.parse::<i32>().unwrap();
        let damage = damage_str.parse::<i32>().unwrap();
        let armor = armor_str.parse::<i32>().unwrap();
        return Ok(Player {
            hit_points,
            damage,
            armor,
        });
    }
    panic!("malformed input");
}

fn weapons(store: &[Item]) -> Vec<Item> {
    store
        .iter()
        .filter(|x| x.kind == ItemType::Weapon)
        .cloned()
        .collect()
}

fn armor(store: &[Item]) -> Vec<Item> {
    store
        .iter()
        .filter(|x| x.kind == ItemType::Armor)
        .cloned()
        .collect()
}

fn rings(store: &[Item]) -> Vec<Item> {
    store
        .iter()
        .filter(|x| x.kind == ItemType::Ring)
        .cloned()
        .collect()
}

fn compute_player(items: &[Item]) -> Player {
    Player {
        hit_points: 100,
        damage: items.iter().map(|x| x.damage).sum(),
        armor: items.iter().map(|x| x.armor).sum(),
    }
}

fn battle(player: &Player, boss: &Player) -> Outcome {
    let player_damage_taken_per_turn = cmp::max(1, boss.damage - player.armor);
    let boss_damage_taken_per_turn = cmp::max(1, player.damage - boss.armor);
    let turns_to_kill_player =
        (player.hit_points as f64 / player_damage_taken_per_turn as f64).ceil();
    let turns_to_kill_boss = (boss.hit_points as f64 / boss_damage_taken_per_turn as f64).ceil();
    if turns_to_kill_player < turns_to_kill_boss {
        Outcome::Die
    } else {
        Outcome::Win
    }
}

fn loadouts(store: &[Item]) -> Vec<Vec<Item>> {
    let weapons = weapons(store);
    let armors = armor(store);
    let rings = rings(store);
    let armor_combos = subsets(&armors, 0, 1);
    let ring_combos = subsets(&rings, 0, 2);
    let mut result = vec![];
    for w in &weapons {
        for a in &armor_combos {
            for r in &ring_combos {
                let mut loadout = vec![w.clone()];
                loadout.extend(a.iter().cloned());
                loadout.extend(r.iter().cloned());
                result.push(loadout);
            }
        }
    }
    result
}

fn subsets<T: Clone>(items: &[T], min: usize, max: usize) -> Vec<Vec<T>> {
    let mut result = Vec::new();
    for size in min..=max {
        combine(items, size, &mut Vec::new(), 0, &mut result);
    }
    result
}

fn combine<T: Clone>(
    items: &[T],
    size: usize,
    current: &mut Vec<T>,
    index: usize,
    result: &mut Vec<Vec<T>>,
) {
    if current.len() == size {
        result.push(current.clone());
        return;
    }
    for i in index..items.len() {
        current.push(items[i].clone());
        combine(items, size, current, i + 1, result);
        current.pop();
    }
}

fn losing_loadouts(store: &[Item], boss: &Player) -> Vec<Vec<Item>> {
    loadouts(store)
        .into_iter()
        .filter(|loadout| {
            let player = compute_player(loadout);
            battle(&player, boss) == Outcome::Die
        })
        .collect()
}

fn loadout_cost(loadout: &[Item]) -> i32 {
    loadout.iter().map(|item| item.cost).sum()
}

fn process_file(filename: &str) -> io::Result<i32> {
    let boss = parse_file(filename)?;
    let store = init_store();
    let highest_cost = losing_loadouts(&store, &boss)
        .iter()
        .map(|loadout| loadout_cost(loadout))
        .max()
        .unwrap();
    Ok(highest_cost)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    if args.len() < 2 {
        usage(progname);
    }
    let filename = &args[1];
    match process_file(filename) {
        Ok(result) => println!("result = {result}"),
        Err(e) => {
            eprintln!("error while processing file `{filename}': {e}");
            process::exit(1);
        }
    }
}
