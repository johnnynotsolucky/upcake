use crate::assertions::AssertionResult;
use crate::{RequestKey, StatResult, State};
use anyhow::Error;
use serde::Serialize;

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
	fn on_notify(&mut self, key: &RequestKey, event: Event);
}

pub struct NoopObserver;

impl<C> Observer<C> for NoopObserver
where
	C: Serialize + Clone,
{
	fn setup(&mut self, _state: &State<C>) {}

	fn on_notify(&mut self, key: &RequestKey, event: Event) {
		println!("{:?}: {:#?}", key, event);
	}
}
