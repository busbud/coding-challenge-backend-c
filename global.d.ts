type ConfigDefinition = {
  port: number;
};

declare module 'config' {
  const config: ConfigDefinition;
  export default config;
}
