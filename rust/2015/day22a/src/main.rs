use std::cmp::min;
use std::collections::HashMap;
use std::env;
use std::fs;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Player {
    hp: i32,
    mana: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Boss {
    hp: i32,
    damage: i32,
}

#[derive(Clone, Debug)]
enum Effect {
    Damage(i32),
    LifeSteal(i32),
    Armor(i32),
    Recharge(i32),
}

#[derive(Clone, Debug)]
struct Spell {
    effect: Effect,
    cost: i32,
    duration: i32,
}

type SpellBook = HashMap<String, Spell>;

fn spells() -> SpellBook {
    HashMap::from([
        (
            String::from("Magic Missile"),
            Spell {
                effect: Effect::Damage(4),
                cost: 53,
                duration: 0,
            },
        ),
        (
            String::from("Drain"),
            Spell {
                effect: Effect::LifeSteal(2),
                cost: 73,
                duration: 0,
            },
        ),
        (
            String::from("Shield"),
            Spell {
                effect: Effect::Armor(7),
                cost: 113,
                duration: 6,
            },
        ),
        (
            String::from("Poison"),
            Spell {
                effect: Effect::Damage(3),
                cost: 173,
                duration: 6,
            },
        ),
        (
            String::from("Recharge"),
            Spell {
                effect: Effect::Recharge(101),
                cost: 229,
                duration: 5,
            },
        ),
    ])
}

#[derive(Clone, Debug)]
struct GameState {
    player: Player,
    boss: Boss,
    active_spells: Vec<(String, i32)>,
    mana_spent: i32,
}

impl GameState {
    fn damage_boss(&self, n: i32) -> Self {
        let mut s = self.clone();
        s.boss.hp -= n;
        s
    }

    fn damage_player(&self, n: i32) -> Self {
        let mut s = self.clone();
        s.player.hp -= n;
        s
    }

    fn heal_player(&self, n: i32) -> Self {
        let mut s = self.clone();
        s.player.hp += n;
        s
    }

    fn use_mana(&self, n: i32) -> Self {
        let mut s = self.clone();
        s.player.mana -= n;
        s.mana_spent += n;
        s
    }

    fn player_armor(&self, book: &SpellBook) -> i32 {
        self.active_spells
            .iter()
            .filter_map(|(name, _)| book.get(name))
            .filter_map(|sp| match sp.effect {
                Effect::Armor(v) => Some(v),
                _ => None,
            })
            .sum()
    }
}

fn castable(gs: &GameState, book: &SpellBook) -> Vec<String> {
    book.iter()
        .filter_map(|(name, sp)| {
            if sp.cost <= gs.player.mana {
                let remaining = gs
                    .active_spells
                    .iter()
                    .find(|(s, _)| s == name)
                    .map(|(_, d)| *d);
                if sp.duration == 0 || remaining.is_none_or(|r| r <= 1) {
                    return Some(name.clone());
                }
            }
            None
        })
        .collect()
}

fn apply_effects(gs: &GameState, book: &SpellBook) -> GameState {
    let mut state = gs.clone();
    for (name, _) in &gs.active_spells {
        if let Some(sp) = book.get(name) {
            match sp.effect {
                Effect::Damage(n) => {
                    state = state.damage_boss(n);
                }
                Effect::Recharge(n) => {
                    state.player.mana += n;
                }
                _ => {}
            }
        }
    }
    state.active_spells = gs
        .active_spells
        .iter()
        .filter_map(|(s, d)| {
            let newd = d - 1;
            if newd > 0 {
                Some((s.clone(), newd))
            } else {
                None
            }
        })
        .collect();
    state
}

fn cast_spell(name: &str, gs: &GameState, book: &SpellBook) -> GameState {
    let sp = book.get(name).expect("spell not found");
    let mut s = gs.use_mana(sp.cost);

    match (&sp.effect, sp.duration) {
        (Effect::Damage(n), 0) => s = s.damage_boss(*n),
        (Effect::LifeSteal(n), 0) => {
            s = s.damage_boss(*n);
            s = s.heal_player(*n);
        }
        (_, d) if d > 0 => {
            let mut new_active = s.active_spells.clone();
            new_active.insert(0, (name.to_string(), d));
            s.active_spells = new_active;
        }
        _ => panic!("unexpected spell shape"),
    }

    s
}

fn boss_attack(gs: &GameState, book: &SpellBook) -> GameState {
    let armor = gs.player_armor(book);
    let damage = (gs.boss.damage - armor).max(1);
    gs.damage_player(damage)
}

fn take_turns(spell: &str, gs: &GameState, book: &SpellBook) -> GameState {
    let gs1 = apply_effects(gs, book);
    if gs1.boss.hp <= 0 {
        return gs1;
    }
    let gs2 = cast_spell(spell, &gs1, book);
    if gs2.boss.hp <= 0 {
        return gs2;
    }
    let gs3 = apply_effects(&gs2, book);
    if gs3.boss.hp <= 0 {
        return gs3;
    }
    boss_attack(&gs3, book)
}

fn search(mut best: i32, gs: &GameState, book: &SpellBook) -> i32 {
    if gs.player.hp <= 0 {
        return best;
    }
    if gs.boss.hp <= 0 {
        return min(best, gs.mana_spent);
    }
    if gs.mana_spent >= best {
        return best;
    }
    for spell in castable(gs, book).into_iter() {
        let next = take_turns(&spell, gs, book);
        if next.mana_spent >= best {
            continue;
        }
        best = min(best, search(best, &next, book));
    }

    best
}

fn parse_input(content: &str) -> Option<Boss> {
    let mut hp: Option<i32> = None;
    let mut dmg: Option<i32> = None;
    for line in content.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("Hit Points:") {
            hp = rest.trim().parse().ok();
        } else if let Some(rest) = line.strip_prefix("Damage:") {
            dmg = rest.trim().parse().ok();
        }
    }
    match (hp, dmg) {
        (Some(h), Some(d)) => Some(Boss { hp: h, damage: d }),
        _ => None,
    }
}

fn process(content: &str) -> i32 {
    let book = spells();
    let boss = parse_input(content).expect("malformed input");
    let init = GameState {
        player: Player { hp: 50, mana: 500 },
        boss,
        active_spells: Vec::new(),
        mana_spent: 0,
    };
    search(i32::MAX, &init, &book)
}

fn usage(progname: &str) {
    eprintln!("usage: {} <input file>", progname);
    std::process::exit(1);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = &args[0];
    match args.as_slice() {
        [_, filename] => {
            let content = fs::read_to_string(filename).expect("failed to read file");
            let result = process(&content);
            println!("{}", result);
        }
        _ => usage(progname),
    }
}
