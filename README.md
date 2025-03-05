YinYang (阴阳) 是 Bagua 开发框架的配套工具，用于辅助开发 Bagua 项目。

## 功能
- 分析项目代码结构，确保符合 Bagua 的代码规范
- 分析 Bagua 项目的代码，确保 `UseCase` 中可能使用的任何 `Entity` 字段都被加载 （递归查找，可能涉及所有层）
- 生成 `Entity` 的增、删、改 `UseCase`（基础 UseCase） (application 层)
- 根据 UseCase 生成对应的 HTTP Adapter （adapters 层）
- 根据 HTTP Adapter 生成对应的 HTTP router (infrastructure 层)


## 使用
### 通过 VSCode 插件 (推荐)
### 通过命令行

## 基础概念
参考 Uncle Bob 的 [The Clean Architecture](https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html)

### 分层
- application 层：业务逻辑层，包含 `UseCase`
- adapters 层：适配层，包含 `Adapter`
- infrastructure 层：基础设施层，包含 `Router`
- domain 层：领域层，包含 `Entity`

### 依赖方向
- 依赖方向：`infrastructure` -> `adapters` -> `application` -> `domain`
