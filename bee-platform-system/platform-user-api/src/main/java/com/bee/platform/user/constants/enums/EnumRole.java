package com.bee.platform.user.constants.enums;

/**
 * @ClassName EnumRole
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/3/14$ 17:29$
 * @version 1.0.0
 */

public class EnumRole {

    public enum  RoleType {
        superAdmin(1,"超级管理员"),admin(2,"管理员"),user(3,"用户");
        private Integer key;
        private String value;

        RoleType() {
        }

        RoleType(Integer key, String value) {
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
