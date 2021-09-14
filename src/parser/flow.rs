use std::collections::{HashMap, HashSet};

use crate::ir::function::BlockId;
use crate::parser::code::Inst;

pub type Block<'a> = &'a [(u32, Inst)];
pub type PredList = HashMap<BlockId, Vec<BlockId>>;

pub trait FindBlock {
    fn find(&self, start: u32) -> Option<usize>;
    fn get_start(&self, index: usize) -> u32;
}

impl FindBlock for [&[(u32, Inst)]] {
    fn find(&self, start: u32) -> Option<usize> {
        self.binary_search_by_key(&start, |x| x[0].0).ok()
    }

    fn get_start(&self, index: usize) -> u32 {
        self[index][0].0
    }
}

pub fn split_basic_blocks(insts: Block) -> (Vec<Block>, HashMap<u32, Vec<u32>>) {
    let mut splits = HashSet::<u32>::new();
    let mut iter = insts.windows(2);
    while let Some([(_, inst), (next, _)]) = iter.next() {
        match inst {
            Inst::GotoIf(_, _, dest) => {
                splits.insert(*dest);
                splits.insert(*next);
            }
            Inst::Goto(dest) => {
                splits.insert(*dest);
                splits.insert(*next);
            }
            Inst::Jsr(_) | Inst::Ret(_) => {
                splits.insert(*next);
            }
            Inst::Switch(default, targets) => {
                splits.insert(*default);
                splits.insert(*next);
                for (_, dest) in targets.as_ref() {
                    splits.insert(*dest);
                }
            }
            Inst::Return(_) | Inst::AThrow => {
                splits.insert(*next);
            }
            _ => {}
        }
    }
    let mut succs = HashMap::new();
    let mut start = 0;
    let mut iter = insts.windows(2);
    while let Some([(_, inst), (next, _)]) = iter.next() {
        if !splits.contains(next) {
            continue;
        }
        match inst {
            Inst::GotoIf(_, _, dest) => {
                let mut tmp = vec![*next, *dest];
                tmp.dedup();
                succs.insert(start, tmp);
            }
            Inst::Goto(dest) => {
                succs.insert(start, vec![*dest]);
            }
            Inst::Jsr(_) | Inst::Ret(_) => {
                succs.insert(start, vec![]);
            }
            Inst::Switch(default, targets) => {
                let mut tmp = vec![*default];
                for (_, dest) in targets.as_ref() {
                    tmp.push(*dest);
                }
                tmp.sort_unstable();
                tmp.dedup();
                succs.insert(start, tmp);
            }
            Inst::Return(_) | Inst::AThrow => {
                succs.insert(start, vec![]);
            }
            _ => {
                succs.insert(start, vec![*next]);
            }
        };
        start = *next;
    }
    let blocks = insts.group_by(|_, x| !splits.contains(&x.0)).collect();
    (blocks, succs)
}

pub fn sort_reverse_postorder(mut succs: HashMap<u32, Vec<u32>>) -> Option<(Vec<BlockId>, PredList)> {
    let mut order = Vec::new();
    let mut preds = HashMap::new();
    let mut stack = vec![(0, succs.remove(&0)?)];
    preds.insert(BlockId(0), vec![BlockId(!0)]);
    preds.insert(BlockId(!0), vec![]);
    while let Some(mut top) = stack.pop() {
        let id = top.0;
        if let Some(next) = top.1.pop() {
            stack.push(top);
            if let Some(seen) = preds.get_mut(&BlockId(next)) {
                seen.push(BlockId(id));
            } else {
                preds.insert(BlockId(next), vec![BlockId(id)]);
                stack.push((next, succs.remove(&next)?));
            }
        } else {
            order.push(BlockId(id));
        }
    }
    order.reverse();
    Some((order, preds))
}
