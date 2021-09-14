use cached::proc_macro::cached;
use noak::descriptor::{BaseType, MethodDescriptor, TypeDescriptor};
use noak::mutf8::MStr;
use std::sync::Arc;
use ustr::Ustr;

pub type MethodType = (Box<[bool]>, Option<bool>);

fn wide(desc: TypeDescriptor) -> bool {
    desc.dimensions() == 0 && (*desc.base() == BaseType::Long || *desc.base() == BaseType::Double)
}

#[cached]
pub fn parse(desc: Ustr) -> Option<Arc<MethodType>> {
    let input = MStr::from_bytes(desc.as_str().as_bytes()).ok()?;
    let mdesc = MethodDescriptor::parse(input).ok()?;
    let parameters = mdesc.parameters().map(wide).collect();
    let return_type = mdesc.return_type().map(wide);
    Some(Arc::new((parameters, return_type)))
}
