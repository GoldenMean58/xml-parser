#[cfg(test)]
mod tests {
    type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;
    trait Parser<'a, Output> {
        fn parse(&self, input:&'a str) -> ParseResult<'a, Output>;
        fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
        where
            Self: Sized + 'a,
            F: Fn(Output) -> NewOutput + 'a,
            Output: 'a,
            NewOutput: 'a,
        {
            BoxedParser::new(map(self, map_fn))
        }
        fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
        where
            Self: Sized + 'a,
            Output: 'a,
            F: Fn(&Output) -> bool + 'a,
        {
            BoxedParser::new(pred(self, pred_fn))
        }
        fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
        where
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
            NextParser: Parser<'a, NewOutput> + 'a,
            F: Fn(Output) -> NextParser + 'a,
        {
            BoxedParser::new(and_then(self, f))
        }
    }
    impl<'a, F, Output> Parser<'a, Output> for F
    where
        F: Fn(&'a str) -> ParseResult<Output>,
    {
        fn parse(&self, input:&'a str) -> ParseResult<'a, Output> {
            self(input)
        }
    }
    struct BoxedParser<'a, Output> {
        parser: Box<dyn Parser<'a, Output> + 'a >,
    }
    impl<'a, Output> BoxedParser<'a, Output> {
        fn new<P>(parser: P) -> Self 
        where
            P: Parser<'a, Output> + 'a
        {
            BoxedParser {
                parser: Box::new(parser),
            }
        }
    }
    impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
        fn parse(&self, input:&'a str) -> ParseResult<'a, Output> {
            self.parser.parse(input)
        }
    }
    
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
    #[derive(Clone, Debug, PartialEq, Eq)]
    struct Element {
        name: String,
        attributes: Vec<(String, String)>,
        children: Vec<Element>,
    }
    fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, String>
    {
        move |input: &'a str| match input.get(0..expected.len()) {
            Some(next) if next == expected => 
                Ok((&input[expected.len()..], String::from(next))),
            _ => Err(input),
        }
    }
   
    #[test]
    fn literal_parser() {
        let parse_content = match_literal("Hello Joe!");
        assert_eq!(
            Ok(("", "Hello Joe!".to_string())),
            parse_content.parse("Hello Joe!")
        );
        assert_eq!(
            Ok((" Hello Robert!", "Hello Joe!".to_string())),
            parse_content.parse("Hello Joe! Hello Robert!")
        );
        assert_eq!(
            Err("Hello Mike!"),
            parse_content.parse("Hello Mike!")
        );
    }
    fn identifier(input: &str) -> ParseResult<String> {
        let mut matched = String::new();
        let mut chars = input.chars();
        match chars.next() {
            Some(next) if next.is_alphabetic() => {
                matched.push(next);
            },
            _ => return Err(&input),
        };
        while let Some(next) = chars.next() {
            if next.is_alphabetic() || next == '-' {
                matched.push(next);
            } else {
                break;
            }
        }
        let next_index = matched.len();
        let last = matched.chars().last();
        match last {
            Some('-') => {
                return Err(&input);
            }
            _ => Ok((&input[next_index..], matched))
        }
    }
    #[test]
    fn identifier_parser() {
        assert_eq!(
            Ok(("", "i-am-an-identifier".to_string())),
            identifier("i-am-an-identifier")
        );
        assert_eq!(
            Ok((" entirely an identifier", "not".to_string())),
            identifier("not entirely an identifier")
        );
        assert_eq!(
            Err("!not at all an identifier"),
            identifier("!not at all an identifier")
        );
        assert_eq!(
            Err("not-an-identifier-"),
            identifier("not-an-identifier-")
        );
    }
    fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
    where 
        P: Parser<'a, A>,
        F: Fn(A) -> B
    {
        move |input| parser.parse(input).map(|(next_input, result)| (next_input, map_fn(result)))
    }
    fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>
    {
        move |input| parser1.parse(input).and_then(|(next_input, result1)| {
            parser2.parse(next_input).map(|(last_input,result2)| (last_input, (result1, result2)))
        })
    }
    fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
    {
        map(pair(parser1, parser2), |(left, _right)| left)
    }
    fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
    {
        map(pair(parser1, parser2), |(_left, right)| right)
    }
    #[test]
    fn pair_combinator() {
        let tag_opener = pair(match_literal("<"), identifier);
        assert_eq!(Ok(("/>", ("<".to_string(), "hello-world".to_string()) )), tag_opener.parse("<hello-world/>"));
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("-oops"), tag_opener.parse("-oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("!oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }
    #[test]
    fn left_combinator() {
        let tag_opener = left(match_literal("<"), identifier);
        assert_eq!(Ok(("/>", "<".to_string())), tag_opener.parse("<hello-world/>"));
    }
    #[test]
    fn right_combinator() {
        let tag_opener = right(match_literal("<"), identifier);
        assert_eq!(Ok(("/>", "hello-world".to_string())), tag_opener.parse("<hello-world/>"));
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }
    fn zero_or_once<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
        P: Parser<'a, A>,
    {
        move |mut input| {
            let mut result = Vec::new();
            if let Ok((next_input, first_item)) = parser.parse(input) {
                input = next_input;
                result.push(first_item);
            }
            Ok((input,result))
        }
    }
    fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
        P: Parser<'a, A>,
    {
        move |mut input| {
            let mut result = Vec::new();
            while let Ok((next_input, first_item)) = parser.parse(input) {
                input = next_input;
                result.push(first_item);
            }
            Ok((input,result))
        }
    }
    /*
    fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
        P: Parser<'a, A>,
    {
        map(pair(parser, zero_or_more(parser)), |(head, mut tail)| {
            tail.insert(0, head);
            tail
        })
    }
    */
    fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>> // +
    where
        P: Parser<'a, A>,
    {
        move |mut input| {
            let mut result = Vec::new();
            if let Ok((next_input, first_item)) = parser.parse(input) {
                input = next_input;
                result.push(first_item);
            } else {
                return Err(input);
            }
            while let Ok((next_input, first_item)) = parser.parse(input) {
                input = next_input;
                result.push(first_item);
            }
            Ok((input,result))
        }
    }
    #[test]
    fn one_or_more_combinator() {
        let parser = one_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec!["ha".to_string(), "ha".to_string(), "ha".to_string()])), parser.parse("hahaha"));
        assert_eq!(Err("ahah"), parser.parse("ahah"));
        assert_eq!(Err(""), parser.parse(""));
    }
    #[test]
    fn zero_or_once_combinator() {
        let parser = zero_or_once(match_literal("ha"));
        assert_eq!(Ok(("haha", vec!["ha".to_string()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }
    #[test]
    fn zero_or_more_combinator() {
        let parser = zero_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec!["ha".to_string(), "ha".to_string(), "ha".to_string()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }
    fn any_char(input: &str) -> ParseResult<char> {
        match input.chars().next() {
            Some(next) => Ok((&input[next.len_utf8()..], next)),
            _ => Err(input),
        }
    }
    fn pred<'a, P, F, A>(parser: P, predicate: F) -> impl Parser<'a, A>
    where
        P: Parser<'a, A>,
        F: Fn(&A) -> bool,
    {
        move |input| {
            if let Ok((next_input, value)) = parser.parse(input) {
                if predicate(&value) {
                    return Ok((next_input, value));
                }
            }
            Err(input)
        }
    }
    
    #[test]
    fn predicate_combinator() {
        let parser = pred(any_char, |c| *c == 'o');
        assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
        assert_eq!(Err("lol"), parser.parse("lol"));
    }
    fn whitespace_char<'a>() -> impl Parser<'a, char> {
        pred(any_char, |c| c.is_whitespace())
    }
    #[test]
    fn whitespace_combinator() {
        let parser = whitespace_char();
        assert_eq!(Ok(("hah", ' ')), parser.parse(" hah"));
        assert_eq!(Err("hah"), parser.parse("hah"));
    }
    fn space0<'a>() -> impl Parser<'a, Vec<char>> {
        zero_or_more(whitespace_char())
    }
    fn space1<'a>() -> impl Parser<'a, Vec<char>> {
        one_or_more(whitespace_char())
    }
    #[test]
    fn space_combinator() {
        let parser = space0();
        assert_eq!(Ok(("hah", vec![])), parser.parse("hah"));
        assert_eq!(Ok(("hah", vec![' '])), parser.parse(" hah"));
        assert_eq!(Ok(("hah", vec![' ', ' '])), parser.parse("  hah"));
        let parser = space1();
        assert_eq!(Err("hah"), parser.parse("hah"));
        assert_eq!(Ok(("hah", vec![' '])), parser.parse(" hah"));
        assert_eq!(Ok(("hah", vec![' ', ' '])), parser.parse("  hah"));
    }
    fn quoted_string<'a>() -> impl Parser<'a, String> {
        right(
            match_literal("\""), 
            left(
                zero_or_more(any_char.pred( |c| *c != '"')), 
                match_literal("\"")
            )
        ) 
        .map(|chars| chars.into_iter().collect())
    }
    #[test]
    fn quoted_string_combinator() {
        let parser = quoted_string();
        assert_eq!(Ok(("", "hah".to_string())), parser.parse("\"hah\""));
        assert_eq!(Ok(("", " hah".to_string())), parser.parse("\" hah\""));
        assert_eq!(Ok(("", "hah ".to_string())), parser.parse("\"hah \""));
        assert_eq!(Ok(("", " hah ".to_string())), parser.parse("\" hah \""));
        assert_eq!(Ok(("", " h ah ".to_string())), parser.parse("\" h ah \""));
        assert_eq!(Err(""), parser.parse("\"hah"));
        assert_eq!(Err("hah\""), parser.parse("hah\""));
    }
    fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
        pair(identifier, right(match_literal("="), quoted_string()))
    }
    fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
        zero_or_more(right(space1(), attribute_pair()))
    }
    #[test]
    fn attribute_parser() {
        assert_eq!(
            Ok((
                "",
                vec![
                    ("one".to_string(), "1".to_string()),
                    ("two".to_string(), "2".to_string())
                ]
            )),
            attributes().parse(" one=\"1\" two=\"2\"")
        );
    }
    fn ele_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
        right(match_literal("<"), pair(identifier, attributes()))
    }
    fn single_ele<'a>() -> impl Parser<'a, Element> {
        left(ele_start(), match_literal("/>")).map(
            |(name, attributes)| Element {
                name,
                attributes,
                children: vec![],
            }
        )
    }
    #[test]
    fn single_element_parser() {
        assert_eq!(
            Ok((
                "",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![]
                }
            )),
            single_ele().parse("<div class=\"float\"/>")
        );
    }
    fn open_ele<'a>() -> impl Parser<'a, Element> {
        left(ele_start(), match_literal(">")).map(|(name, attributes)| Element {
            name,
            attributes,
            children: vec![],
        })
    }
    fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
    where
        P1: Parser<'a, A>,
        P2: Parser<'a, A>,
    {
        move |input| match parser1.parse(input) {
            ok @ Ok(_) => ok,
            Err(_) => parser2.parse(input),
        }
    }
    fn whiltespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
    where
        P: Parser<'a, A>,
    {
        right(space0(), left(parser, space0()))
    }
    fn element<'a>() -> impl Parser<'a, Element> {
        whiltespace_wrap(either(single_ele(), parent_ele()))
    }
    fn close_ele<'a>(expected_name: String) -> impl Parser<'a, String> {
        right(
            match_literal("</"), 
                left(
                    identifier, 
                    match_literal(">")
                )
        )
        .pred(move |name| name == &expected_name)
    }
    fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        NextP: Parser<'a, B>,
        F: Fn(A) -> NextP,
    {
        move |input| match parser.parse(input) {
            Ok((next_input, result)) => f(result).parse(next_input),
            Err(err) => Err(err),
        }
    }
    fn parent_ele<'a>() -> impl Parser<'a, Element> {
        open_ele().and_then(|el| {
            left(zero_or_more(element()), close_ele(el.name.clone())).map(move |children| {
                let mut el = el.clone();
                el.children = children;
                el
            })
        })
    }
    #[test]
    fn xml_parser() {
        let doc = r#"
            <top label="Top">
                <semi-bottom label="Bottom"/>
                <middle>
                    <bottom label="Another bottom"/>
                </middle>
            </top>"#;
        let parsed_doc = Element {
            name: "top".to_string(),
            attributes: vec![("label".to_string(), "Top".to_string())],
            children: vec![
                Element {
                    name: "semi-bottom".to_string(),
                    attributes: vec![("label".to_string(), "Bottom".to_string())],
                    children: vec![],
                },
                Element {
                    name: "middle".to_string(),
                    attributes: vec![],
                    children: vec![Element {
                        name: "bottom".to_string(),
                        attributes: vec![("label".to_string(), "Another bottom".to_string())],
                        children: vec![],
                    }],
                },
            ],
        };
        assert_eq!(Ok(("", parsed_doc)), element().parse(doc));
    }
    #[test]
    fn mismatched_closing_tag() {
        let doc = r#"
            <top>
                <bottom/>
            </middle>"#;
        assert_eq!(Err("</middle>"), element().parse(doc));
    }
}
