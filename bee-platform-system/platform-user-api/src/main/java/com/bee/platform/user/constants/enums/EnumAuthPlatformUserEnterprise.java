package com.bee.platform.user.constants.enums;




public class EnumAuthPlatformUserEnterprise {
	public enum  Status {
    /**
     *  用户状态
     */
    not_status(0,"已禁用"),
    is_status(1,"启用中");
	private Integer key;
    private String value;
    Status(){
    	
    }
    Status(Integer key, String value) {
        this.key = key;
        this.value = value;
    }

    public Integer getKey() {
        return key;
    }

    public void setKey(Integer key) {
        this.key = key;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
	
	}
}
