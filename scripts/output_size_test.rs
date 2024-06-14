enum Output {
    Flag,
    One(String),
    Two((String, String)),
    Three((String, String, String)),
    Four((String, String, String)),
    Five((String, String, String, String, String)),
    More(Vec<String>),
}

impl Output {
    fn from(slice: &[String]) -> Output {
        todo()
    }

    fn contains(&self, x: String) -> bool {
        todo!()
    }

    fn len(&self) -> usize {
        todo()
    }
}

// TODO compare hashset<str> vec<str> and tuple<str> look up times
// TODO use output for flags and holding arb vals