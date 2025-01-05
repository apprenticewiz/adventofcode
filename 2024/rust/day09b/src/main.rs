use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::process;
use std::time::SystemTime;

type DiskImage = (BTreeMap<u32, (u32, u32)>, BTreeMap<u32, u32>);

fn usage() {
    let progname = env::args().next().unwrap();
    println!("usage: {progname} <file>");
    process::exit(1);
}

fn scan_filemap(contents: &str) -> DiskImage {
    let trimmed = contents.trim_end();
    let mut disk_map: BTreeMap<u32, (u32, u32)> = BTreeMap::new();
    let mut free_list: BTreeMap<u32, u32> = BTreeMap::new();
    let mut i: usize = 0;
    let mut pos: u32 = 0;
    let mut file_id: u32 = 0;
    loop {
        let size = trimmed.chars().nth(i).unwrap().to_digit(10).unwrap();
        disk_map.insert(pos, (size, file_id));
        if i + 1 == trimmed.len() {
            break;
        }
        pos += size;
        let skip = trimmed.chars().nth(i + 1).unwrap().to_digit(10).unwrap();
        if skip != 0 {
            free_list.insert(pos, skip);
        }
        pos += skip;
        file_id += 1;
        i += 2;
    }
    (disk_map, free_list)
}

fn build_filemap(disk_map: &BTreeMap<u32, (u32, u32)>) -> BTreeMap<u32, u32> {
    let mut filemap: BTreeMap<u32, u32> = BTreeMap::new();
    for (file_start, (_, file_id)) in disk_map.iter().rev() {
        filemap.insert(*file_id, *file_start);
    }
    filemap
}

fn first_available_gap(free_list: &BTreeMap<u32, u32>, pos: u32, size: u32) -> Option<u32> {
    for (gap_start, gap_size) in free_list.iter() {
        if *gap_start > pos {
            return None;
        } else if size <= *gap_size {
            return Some(*gap_start);
        }
    }
    None
}

fn compact(disk_image: &DiskImage) -> DiskImage {
    let mut working_map = disk_image.0.clone();
    let mut working_free = disk_image.1.clone();
    let mut filemap = build_filemap(&working_map);
    while let Some((file_id, file_start)) = filemap.pop_last() {
        let (file_size, _) = working_map.get(&file_start).unwrap();
        let mut temp_map = working_map.clone();
        if let Some(gap_start) = first_available_gap(&working_free, file_start, *file_size) {
            temp_map.remove(&file_start);
            temp_map.insert(gap_start, (*file_size, file_id));
            let (_, gap_size) = working_free.remove_entry(&gap_start).unwrap();
            if gap_size > *file_size {
                working_free.insert(gap_start + file_size, gap_size - file_size);
            }
        }
        working_map = temp_map;
    }
    (working_map, working_free)
}

fn calc_checksum(disk_image: &DiskImage) -> u64 {
    let mut working_map = disk_image.0.clone();
    let mut checksum: u64 = 0;
    while let Some((file_start, (file_size, file_id))) = working_map.pop_first() {
        for i in 0..file_size {
            checksum += ((file_start + i) * file_id) as u64;
        }
    }
    checksum
}

fn process(contents: &str) -> u64 {
    let disk_image = scan_filemap(contents);
    let compact_image = compact(&disk_image);
    calc_checksum(&compact_image)
}

fn calc_runtime(start_time: &SystemTime) -> String {
    let mut elapsed = start_time.elapsed().unwrap().as_micros() as f64;
    let mut units = "μs";
    if elapsed > 1000.0 {
        elapsed /= 1000.0;
        units = "ms";
    }
    if elapsed > 1000.0 {
        elapsed /= 1000.0;
        units = "s";
    }
    format!("(elapsed time: {elapsed} {units})")
}

fn main() {
    let start_time = SystemTime::now();
    if env::args().count() < 2 {
        usage();
    }
    let filename = env::args().nth(1).unwrap();
    let contents = fs::read_to_string(filename).expect("read of input file failed");
    let result = process(&contents);
    let duration = calc_runtime(&start_time);
    println!("result = {result}  {duration}");
}
