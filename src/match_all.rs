use crate::{
    matcher::FindAllPartMatch,
    token_entry::{Source, TokenStringBuilder},
    utils::is_empty,
    Rule,
};
use std::{mem::take, ops::Range};

#[derive(Debug)]
pub(crate) struct MatchAllBuilder<'a> {
    source: Source<'a>,
    parts: Vec<RawPart>,
    changes: Vec<RawChangedPart>,

    no_match_tes_start: usize,
    no_match_tes_end: usize,
}

impl<'a> MatchAllBuilder<'a> {
    pub fn new(source: Source<'a>) -> Self {
        Self {
            source,
            parts: Vec::new(),
            changes: Vec::new(),
            no_match_tes_start: 0,
            no_match_tes_end: 0,
        }
    }
    pub fn push_no_match(&mut self, tes_len: usize) {
        println!("push_no_match({})", tes_len);
        self.no_match_tes_end += tes_len;
    }
    fn commit_no_match(&mut self) {
        let tes_range = self
            .source
            .unchanged_tes_range_in(self.no_match_tes_start..self.no_match_tes_end);
        self.push_transform(self.no_match_tes_start..tes_range.start);
        self.push_unchanged(tes_range.clone());
        self.push_transform(tes_range.end..self.no_match_tes_end);
        let tes_offset = self.no_match_tes_end;
        self.no_match_tes_start = tes_offset;
        self.no_match_tes_end = tes_offset;
    }
    pub fn push_match(&mut self, m: FindAllPartMatch) {
        print!("push_match({:#?})", m);

        self.commit_no_match();
        let tes_start = self.no_match_tes_end;
        let tes_end = tes_start + m.tes_len;
        self.changes.push(RawChangedPart {
            tes_range: tes_start..tes_end,
            m: Some(m),
        });
        self.no_match_tes_start = tes_end;
        self.no_match_tes_end = tes_end;
    }
    fn push_transform(&mut self, tes_range: Range<usize>) {
        if is_empty(&tes_range) {
            return;
        }
        self.changes.push(RawChangedPart { tes_range, m: None });
    }
    fn push_unchanged(&mut self, tes_range: Range<usize>) {
        if is_empty(&tes_range) {
            return;
        }
        self.commit_changes();
        self.parts.push(RawPart::Unchanged { tes_range });
    }
    fn commit_changes(&mut self) {
        if !self.changes.is_empty() {
            let tes_start = self.changes[0].tes_range.start;
            let tes_end = self.changes[self.changes.len() - 1].tes_range.end;
            self.parts.push(RawPart::Changed(RawChanged {
                tes_range: tes_start..tes_end,
                parts: take(&mut self.changes),
            }));
        }
    }
    pub fn finish(mut self, rule: &'a Rule) -> MatchAll<'a> {
        self.commit_no_match();
        self.commit_changes();
        MatchAll {
            source: self.source,
            rule,
            parts: self.parts,
        }
    }
}

pub struct MatchAll<'a> {
    source: Source<'a>,
    rule: &'a Rule,
    parts: Vec<RawPart>,
}

impl<'a> MatchAll<'a> {
    pub fn iter(&self) -> impl Iterator<Item = Part> {
        self.parts
            .iter()
            .map(|part| match part {
                RawPart::Unchanged { tes_range } => Part::Unchanged(Unchanged {
                    ma: self,
                    tes_range: tes_range.clone(),
                }),
                RawPart::Changed(p) => Part::Changed(Changed { ma: self, p }),
            })
            .collect::<Vec<_>>()
            .into_iter()
    }
}

#[derive(Debug)]
enum RawPart {
    Unchanged { tes_range: Range<usize> },
    Changed(RawChanged),
}

#[derive(Debug)]
struct RawChanged {
    tes_range: Range<usize>,
    parts: Vec<RawChangedPart>,
}
#[derive(Debug)]
struct RawChangedPart {
    tes_range: Range<usize>,
    m: Option<FindAllPartMatch>,
}

pub enum Part<'a> {
    Unchanged(Unchanged<'a>),
    Changed(Changed<'a>),
}

pub struct Unchanged<'a> {
    ma: &'a MatchAll<'a>,
    tes_range: Range<usize>,
}
impl<'a> Unchanged<'a> {
    pub fn source_str(&self) -> &str {
        self.ma.source.get_source(self.tes_range.clone())
    }
}
pub struct Changed<'a> {
    ma: &'a MatchAll<'a>,
    p: &'a RawChanged,
}

impl<'a> Changed<'a> {
    pub fn source_str(&self) -> &str {
        self.ma.source.get_source(self.p.tes_range.clone())
    }
    pub fn replacement_str(&self) -> String {
        let mut b = TokenStringBuilder::new(&self.ma.source);
        for p in &self.p.parts {
            if let Some(m) = &p.m {
                m.apply_string(self.ma.rule, &mut b);
            } else {
                b.push_tes(p.tes_range.clone());
            }
        }
        b.s
    }
    pub fn replacements(&self) -> impl Iterator<Item = Replacement> {
        let ma = &self.ma;
        self.p.parts.iter().map(|p| {
            if let Some(m) = &p.m {
                Replacement::Match(Match { ma, m })
            } else {
                Replacement::Transform(Transform {
                    ma,
                    tes_range: p.tes_range.clone(),
                })
            }
        })
    }
}

pub enum Replacement<'a> {
    Transform(Transform<'a>),
    Match(Match<'a>),
}

pub struct Transform<'a> {
    ma: &'a MatchAll<'a>,
    tes_range: Range<usize>,
}

impl<'a> Transform<'a> {
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let mut b = TokenStringBuilder::new(&self.ma.source);
        b.push_tes(self.tes_range.clone());
        b.s
    }
}

pub struct Match<'a> {
    ma: &'a MatchAll<'a>,
    m: &'a FindAllPartMatch,
}

impl<'a> Match<'a> {
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let mut b = TokenStringBuilder::new(&self.ma.source);
        self.m.apply_string(self.ma.rule, &mut b);
        b.s
    }
}
