use rand::prelude::*;
use rand_chacha::ChaCha20Rng;

const MAX_u64_AS_F64: f64 = std::u64::MAX as f64;

fn get_generator(seed: u64) -> ChaCha20Rng {
    rand_chacha::ChaCha20Rng::seed_from_u64(seed)
}

/*
    This method lives up to the lecture notes PDF's version of multi-jittered sampling.
    According to what is on the web, this is perhaps not the optimal multi-jittered method, because samples are not as evenly distributed.
*/
fn shuffle_multi_jittered(samples: &mut Vec<(f64, f64)>, rng: &mut ChaCha20Rng, n: usize) {
    for i in 0..n - 1 {
        for j in 0..n - 1 {
            let target = (i + rng.gen_range(0..n - i)) * n + j;
            let current = i * n + j;
            samples.swap(current, target);
        }
    }

    for i in 0..n - 1 {
        for j in 0..n - 1 {
            let target = j * n + (i + rng.gen_range(0..n - i));
            let current = j * n + i;
            samples.swap(current, target);
        }
    }
}

/*
    This shuffle method uses correlated shuffling to improve upon the basic idea of multi-jittered sampling. This ensures a more evenly distributed set of samples. 
    
    The change from shuffle_multi_jittered() is that this method shuffles using the same randomly chosen value for an entire row/column, instead of shuffling with a new random value for every sample point.

    Correlated extension is found here: https://graphics.pixar.com/library/MultiJitteredSampling/paper.pdf
*/
fn shuffle_correlated(samples: &mut Vec<(f64, f64)>, rng: &mut ChaCha20Rng, n: usize) {
    for i in 0..n - 1 {
        let k = i + rng.gen_range(0..n - i);
        for j in 0..n - 1 {
            let target = k * n + j;
            let current = i * n + j;
            samples.swap(current, target);
        }
    }

    for i in 0..n - 1 {
        let k = i + rng.gen_range(0..n - i);
        for j in 0..n - 1 {
            let target = j * n + k;
            let current = j * n + i;
            samples.swap(current, target);
        }
    }
} 

/*
    This function sets up the initial diagonal distribution of samples, and calls a shuffling function to perform the multi-jittered shuffling
*/
fn multi_jittered(n: usize, sn: usize) {
    let sets = vec![(0.0, 0.0); sn];
    let ns = n.pow(2);
    let mut k = sn - 1;
    loop {
        let samples = vec![(0.0, 0.0); ns];

    }
}

/*
    Returns a jittered sample point, that lies within a given grid cell, in relation to the max grid value
*/
fn get_jittered_value(cell: i32, max: i32, rng: ChaCha20Rng) {
    //rng.next_u64() as f64 / MAX_u64_AS_F64
}