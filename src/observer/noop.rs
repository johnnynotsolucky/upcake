use anyhow::Result;
use serde::Serialize;

use crate::{RequestKey, State};

use super::{Event, Observer};

pub struct NoopObserver;

impl<C> Observer<C> for NoopObserver
where
	C: Serialize + Clone,
{
	fn setup(&mut self, _state: &State<C>) {}

	fn on_notify(&mut self, _key: &RequestKey, _event: Event) -> Result<()> {
		Ok(())
	}
}
