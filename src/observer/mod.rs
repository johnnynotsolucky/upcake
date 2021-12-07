use crate::assertions::AssertionResult;
use crate::{RequestKey, StatResult, State};
use anyhow::Error;
use anyhow::Result;
use serde::Serialize;

pub mod noop;
pub mod write;

#[derive(Debug)]
pub enum Event<'a> {
	RequestStateChanged(RequestState<'a>),
	AssertionResultAdded(&'a AssertionResult),
}

#[derive(Debug)]
pub enum RequestState<'a> {
	/// Request hasn't started yet
	Pending,
	/// The request future for running requests
	Running,
	/// Request has completed successfully
	Success(&'a StatResult),
	/// Request has completed but with an error
	Error(&'a Error),
}

pub trait Observer<C>
where
	C: Serialize + Clone,
{
	fn setup(&mut self, state: &State<C>);
	fn on_notify(&mut self, key: &RequestKey, event: Event) -> Result<()>;
}
