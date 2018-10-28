#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MuteSettings {
    pub keywords: Vec<String>,
    pub users: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ClientConfig {
    pub bearer: String,
    pub url: String,
    pub targets: Vec<String>,
    pub mute: MuteSettings,
}
