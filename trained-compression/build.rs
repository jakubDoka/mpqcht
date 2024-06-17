#![feature(array_chunks)]
#![feature(iter_intersperse)]
use std::collections::HashMap;

struct DisplayArrayPat<'a>(&'a [u8]);

impl<'a> std::fmt::Display for DisplayArrayPat<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for v in self.0.iter() {
            write!(f, "{v:?}, ")?;
        }
        write!(f, "..]")
    }
}

const QUADS: usize = 16;
const TRIPLES: usize = 8;
const ENCODED_LEN: usize = 256 - 8;

fn build_mappint(data: &[u8], width: usize, count: usize) -> Vec<&[u8]> {
    let mut mapping = HashMap::<_, u32>::new();
    for chunk in data.chunks(width) {
        *mapping.entry(chunk).or_default() += 1;
    }
    let mut top = mapping.into_iter().collect::<Vec<_>>();
    top.sort_by_key(|&(_, f)| u32::MAX - f);
    top.truncate(count);
    top.into_iter().map(|(a, _)| a).collect::<Vec<_>>()
}

fn main() -> std::fmt::Result {
    let file = "./src/trained.rs";
    let data = "dataset.txt";
    println!("cargo::rerun-if-changed={data}");
    println!("cargo:rerun-if-changed=build.rs");

    let mut data = std::fs::read_to_string(data).unwrap();
    data = data.split_whitespace().intersperse(" ").collect();
    let data = data.as_bytes();

    let [top1, top2, top3] = std::thread::scope(|t| {
        [
            t.spawn(|| build_mappint(data, 4, QUADS)),
            t.spawn(|| build_mappint(data, 3, TRIPLES)),
            t.spawn(|| build_mappint(data, 2, ENCODED_LEN - TRIPLES - QUADS)),
        ]
        .map(|t| t.join().unwrap())
    });

    use std::fmt::Write;
    let mut out = String::new();

    writeln!(out, "pub fn dispatch(arr: &[u8]) -> Option<(u8, usize)> {{")?;
    writeln!(out, "\tSome(match arr {{")?;
    for (i, &v) in [&top1, &top2, &top3].into_iter().flatten().enumerate() {
        writeln!(out, "\t\t{} => ({i}, {}),", DisplayArrayPat(v), v.len())?;
    }
    writeln!(out, "\t\t_ => return None,")?;
    writeln!(out, "\t}})")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "pub const LOOKUP4: [[u8; 4]; {QUADS}] = {top1:?};")?;
    writeln!(out, "pub const LOOKUP3: [[u8; 3]; {TRIPLES}] = {top2:?};")?;
    writeln!(
        out,
        "pub const LOOKUP2: [[u8; 2]; {}] = {top3:?};",
        ENCODED_LEN - TRIPLES - QUADS
    )?;

    std::fs::write(file, out).unwrap();
    Ok(())
}
