#[derive(Debug)]
struct Config {
    max_bookmarks: usize,
}

#[derive(Debug)]
struct User {
    config: Config,
    bookmarks: Vec<String>,
}

fn main() {
    let mut user = User {
        bookmarks: vec![],
        config: Config {
            max_bookmarks: 1000,
        },
    };

    let config = &user.config;
    println!("{:?} {:?}", user, config);
}
