use rand::Rng;

pub fn generate_node_id() -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                            abcdefghijklmnopqrstuvwxyz\
                            ";
    const id_len: usize = 15;
    let mut rng = rand::thread_rng();

    let id: String = (0..id_len)
        .map(|_| {
            let idx = rng.gen_range(0..CHARSET.len());
            CHARSET[idx] as char
        })
        .collect();

    return id;
}