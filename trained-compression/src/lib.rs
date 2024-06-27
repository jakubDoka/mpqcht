#![feature(array_windows)]
#![feature(slice_from_ptr_range)]
#![feature(array_chunks)]
#![feature(slice_take)]
#![feature(portable_simd)]
#![feature(iter_intersperse)]

mod trained;

#[cfg(test)]
#[test]
fn main() {
    let data = std::fs::read_to_string("dataset.txt").unwrap();

    let now = std::time::Instant::now();
    let compressed = compress(data.as_bytes());

    println!(
        "{} / {} = {} in {:?} {:?}",
        compressed.len(),
        data.len(),
        1.0 - (compressed.len() as f64 / data.len() as f64),
        now.elapsed().checked_div(data.len() as _).unwrap(),
        now.elapsed()
    );

    let now = std::time::Instant::now();
    let decompressed = decompress(compressed.as_slice()).unwrap();

    println!(
        "{:?} {:?}",
        now.elapsed().checked_div(compressed.len() as _).unwrap(),
        now.elapsed(),
    );

    assert!(data.as_bytes() == decompressed);

    let test = b"Just before a package is built, Cargo will compile a build script into an executable (if it has not already been built). It will then run the script, which may perform any number of tasks. The script may communicate with Cargo by printing specially formatted commands prefixed with cargo:: to stdout. The build script will be rebuilt if any of its source files or dependencies change. By default, Cargo will re-run the build script if any of the files in the package changes. Typically it is best to use the rerun-if commands, described in the change detection section below, to narrow the focus of what triggers a build script to run again. Once the build script successfully finishes executing, the rest of the package will be compiled. Scripts should exit with a non-zero exit code to halt the build if there is an error, in which case the build script's output will be displayed on the terminal.";

    let compressed = compress(test);

    println!(
        "{} / {} = {}",
        compressed.len(),
        test.len(),
        1.0 - (compressed.len() as f64 / test.len() as f64),
    );
}

const CHUNK_SIZE: usize = 2;
const QUADS: usize = trained::LOOKUP4.len();
const TRIPLES: usize = trained::LOOKUP3.len();
const ENCODED_LEN: usize = trained::LOOKUP2.len() + TRIPLES + QUADS;

pub fn compress(data: &[u8]) -> Vec<u8> {
    let mut compressed = Vec::new();

    fn eat_unencoded(last_unencoded: &[u8], compressed: &mut Vec<u8>) {
        for chunk in last_unencoded.chunks(256 - ENCODED_LEN) {
            compressed.push(chunk.len() as u8 - 1 + ENCODED_LEN as u8);
            compressed.extend(chunk);
        }
    }

    let mut cursor = data.as_ptr_range();
    let mut last_unencoded = cursor.start;
    while cursor.start < unsafe { cursor.end.sub(CHUNK_SIZE - 1) } {
        let rest = unsafe { std::slice::from_ptr_range(cursor.start..cursor.end) };
        if let Some((j, len)) = trained::dispatch(rest) {
            eat_unencoded(
                unsafe { std::slice::from_ptr_range(last_unencoded..cursor.start) },
                &mut compressed,
            );
            compressed.push(j);
            cursor.start = unsafe { cursor.start.add(len) };
            last_unencoded = cursor.start;
        } else {
            cursor.start = unsafe { cursor.start.add(1) };
        }
    }

    eat_unencoded(
        unsafe { std::slice::from_ptr_range(last_unencoded..cursor.end) },
        &mut compressed,
    );

    compressed
}

pub fn decompress(mut data: &[u8]) -> Option<Vec<u8>> {
    let mut decompressed = Vec::new();
    while let Some(&i) = data.take_first() {
        if i < QUADS as u8 {
            decompressed.extend(trained::LOOKUP4[i as usize]);
        } else if i < TRIPLES as u8 + QUADS as u8 {
            decompressed.extend(trained::LOOKUP3[i as usize - QUADS]);
        } else if i < ENCODED_LEN as u8 {
            decompressed.extend(trained::LOOKUP2[i as usize - TRIPLES - QUADS]);
        } else {
            let len = i - ENCODED_LEN as u8 + 1;
            decompressed.extend(data.take(..len as usize)?);
        }
    }
    Some(decompressed)
}
