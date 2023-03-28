use std::collections::HashMap;

use anyhow::bail;
use cosmwasm_std::{Addr, Api, Binary, BlockInfo, CosmosMsg, CustomQuery, Empty, Querier, Storage};

use crate::{AppResponse, CosmosRouter};

// TODO: turn into extensions of Fn trait
pub trait StargateQueryHandler {
    fn query(
        &self,
        api: &dyn Api,
        storage: &dyn Storage,
        querier: &dyn Querier,
        block: &BlockInfo,
        request: StargateMsg,
    ) -> anyhow::Result<Binary>;
}

pub trait StargateMessageHandler<ExecC, QueryC: CustomQuery> {
    fn execute(
        &self,
        api: &dyn Api,
        storage: &mut dyn Storage,
        router: &dyn CosmosRouter<ExecC = ExecC, QueryC = QueryC>,
        block: &BlockInfo,
        sender: Addr,
        msg: StargateMsg,
    ) -> anyhow::Result<AppResponse>;
}

pub struct StargateKeeper<ExecC: 'static, QueryC: 'static> {
    messages: HashMap<String, &'static dyn StargateMessageHandler<ExecC, QueryC>>,
    queries: HashMap<String, &'static dyn StargateQueryHandler>,
}

impl<ExecC, QueryC> StargateKeeper<ExecC, QueryC> {
    pub fn new() -> Self {
        Self {
            messages: HashMap::new(),
            queries: HashMap::new(),
        }
    }

    pub fn register_msg(
        &mut self,
        type_url: &str,
        handler: &'static dyn StargateMessageHandler<ExecC, QueryC>,
    ) {
        self.messages.insert(
            type_url.to_string(),
            handler as &'static dyn StargateMessageHandler<ExecC, QueryC>,
        );
    }

    pub fn register_query(&mut self, type_url: &str, handler: &'static dyn StargateQueryHandler) {
        self.queries
            .insert(type_url.to_string(), handler as &dyn StargateQueryHandler);
    }
}

pub struct StargateMsg {
    pub type_url: String,
    pub value: Binary,
}

impl From<StargateMsg> for CosmosMsg {
    fn from(msg: StargateMsg) -> Self {
        CosmosMsg::Stargate {
            type_url: msg.type_url,
            value: msg.value,
        }
    }
}

pub trait Stargate<ExecC, QueryC> {
    fn execute(
        &self,
        api: &dyn Api,
        storage: &mut dyn Storage,
        router: &dyn CosmosRouter<ExecC = ExecC, QueryC = QueryC>,
        block: &BlockInfo,
        sender: Addr,
        msg: StargateMsg,
    ) -> anyhow::Result<crate::AppResponse>;

    fn sudo(
        &self,
        api: &dyn Api,
        storage: &mut dyn Storage,
        router: &dyn CosmosRouter<ExecC = ExecC, QueryC = QueryC>,
        block: &cosmwasm_std::BlockInfo,
        msg: Empty,
    ) -> anyhow::Result<crate::AppResponse>;

    fn query(
        &self,
        api: &dyn cosmwasm_std::Api,
        storage: &dyn cosmwasm_std::Storage,
        querier: &dyn cosmwasm_std::Querier,
        block: &cosmwasm_std::BlockInfo,
        request: StargateMsg,
    ) -> anyhow::Result<cosmwasm_std::Binary>;
}

impl<ExecC, QueryC: CustomQuery> Stargate<ExecC, QueryC> for StargateKeeper<ExecC, QueryC> {
    fn execute(
        &self,
        api: &dyn Api,
        storage: &mut dyn Storage,
        router: &dyn CosmosRouter<ExecC = ExecC, QueryC = QueryC>,
        block: &BlockInfo,
        sender: Addr,
        msg: StargateMsg,
    ) -> anyhow::Result<crate::AppResponse> {
        match self.messages.get(&msg.type_url.to_string()) {
            Some(handler) => handler.execute(api, storage, router, block, sender, msg),
            None => bail!("Unsupported stargate message: {}", msg.type_url),
        }
    }

    fn sudo(
        &self,
        _api: &dyn Api,
        _storage: &mut dyn Storage,
        _router: &dyn CosmosRouter<ExecC = ExecC, QueryC = QueryC>,
        _block: &cosmwasm_std::BlockInfo,
        _msg: Empty,
    ) -> anyhow::Result<crate::AppResponse> {
        bail!("StargateKeeper does not support sudo")
    }

    fn query(
        &self,
        api: &dyn cosmwasm_std::Api,
        storage: &dyn cosmwasm_std::Storage,
        querier: &dyn cosmwasm_std::Querier,
        block: &cosmwasm_std::BlockInfo,
        request: StargateMsg,
    ) -> anyhow::Result<cosmwasm_std::Binary> {
        println!("StargateKeeper::query");
        match self.queries.get(&request.type_url.to_string()) {
            Some(handler) => {
                println!("StargateKeeper::query: found handler");
                handler.query(api, storage, querier, block, request)
            }
            None => bail!("Unsupported stargate query: {}", request.type_url),
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Ok;
    use cosmwasm_std::{from_binary, to_binary, CosmosMsg, Event, QueryRequest};

    use crate::{BasicAppBuilder, Executor};

    use super::*;

    struct FooHandler;
    impl StargateMessageHandler<Empty, Empty> for FooHandler {
        fn execute(
            &self,
            _api: &dyn Api,
            _storage: &mut dyn Storage,
            _router: &dyn CosmosRouter<ExecC = Empty, QueryC = Empty>,
            _block: &BlockInfo,
            _sender: Addr,
            msg: StargateMsg,
        ) -> anyhow::Result<AppResponse> {
            let mut res = AppResponse::default();
            let num: u64 = from_binary(&msg.value)?;
            res.events
                .push(Event::new("foo").add_attribute("bar", num.to_string()));
            Ok(res)
        }
    }
    const FOO_HANDLER: FooHandler = FooHandler;

    struct FooQueryHandler;
    impl StargateQueryHandler for FooQueryHandler {
        fn query(
            &self,
            _api: &dyn Api,
            _storage: &dyn Storage,
            _querier: &dyn Querier,
            _block: &BlockInfo,
            msg: StargateMsg,
        ) -> anyhow::Result<Binary> {
            let num: u64 = from_binary(&msg.value)?;
            let bin = to_binary(&format!("bar{:?}", num)).unwrap();
            println!("post bin conversion");
            Ok(bin)
        }
    }
    const FOO_QUERY_HANDLER: FooQueryHandler = FooQueryHandler;

    struct BarHandler;
    impl StargateMessageHandler<Empty, Empty> for BarHandler {
        fn execute(
            &self,
            api: &dyn Api,
            storage: &mut dyn Storage,
            router: &dyn CosmosRouter<ExecC = Empty, QueryC = Empty>,
            block: &BlockInfo,
            _sender: Addr,
            msg: StargateMsg,
        ) -> anyhow::Result<AppResponse> {
            let query_res = router.query(
                api,
                storage,
                block,
                QueryRequest::Stargate {
                    path: "foo".to_string(),
                    data: msg.value,
                },
            )?;

            let mut res = AppResponse::default();
            res.data = Some(query_res);

            Ok(res)
        }
    }
    const BAR_HANDLER: BarHandler = BarHandler;

    #[test]
    fn new_stargate_keeper() {
        StargateKeeper::<Empty, Empty>::new();
    }

    #[test]
    fn register_and_call_stargate_msg() {
        let mut stargate_keeper = StargateKeeper::new();
        stargate_keeper.register_msg("foo", &FOO_HANDLER);

        let mut app = BasicAppBuilder::<Empty, Empty>::new()
            .with_stargate(stargate_keeper)
            .build(|_, _, _| {});

        let res = app
            .execute(
                Addr::unchecked("unchecked"),
                CosmosMsg::Stargate {
                    type_url: "foo".to_string(),
                    value: Binary::default(),
                },
            )
            .unwrap();

        res.assert_event(&Event::new("foo").add_attribute("bar", "bar"));
    }

    #[test]
    fn register_and_call_stargate_query() {
        let mut stargate_keeper = StargateKeeper::new();
        stargate_keeper.register_query("foo", &FOO_QUERY_HANDLER);

        let app = BasicAppBuilder::<Empty, Empty>::new()
            .with_stargate(stargate_keeper)
            .build(|_, _, _| {});

        let querier = app.wrap();

        let res: String = querier
            .query(&QueryRequest::Stargate {
                path: "foo".to_string(),
                data: to_binary(&1337u64).unwrap(),
            })
            .unwrap();

        assert_eq!(res, "bar1337".to_string());
    }

    #[test]
    fn query_inside_execution() {
        let mut stargate_keeper = StargateKeeper::new();
        stargate_keeper.register_msg("bar", &BAR_HANDLER);
        stargate_keeper.register_query("foo", &FOO_QUERY_HANDLER);

        let mut app = BasicAppBuilder::<Empty, Empty>::new()
            .with_stargate(stargate_keeper)
            .build(|_, _, _| {});

        let res = app
            .execute(
                Addr::unchecked("unchecked"),
                CosmosMsg::Stargate {
                    type_url: "bar".to_string(),
                    value: to_binary(&1337u64).unwrap(),
                },
            )
            .unwrap();

        let x: String = from_binary(&res.data.unwrap()).unwrap();
        assert_eq!(x, "bar1337");
    }
}
