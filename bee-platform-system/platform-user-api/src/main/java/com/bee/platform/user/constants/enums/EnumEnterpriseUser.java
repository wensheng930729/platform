package com.bee.platform.user.constants.enums;

/**
 * @ClassName EnumEnterpriseUser
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/3/19$ 10:47$
 * @version 1.0.0
 */

public class EnumEnterpriseUser {
    public enum  ActiveType {
        not_active(0,"未激活"),is_active(1,"已激活");
        private Integer key;
        private String value;

        ActiveType() {
        }

        ActiveType(Integer key, String value) {
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

    public enum  InviteType {
        not_invite(0,"未被邀请"),is_invite(1,"已被邀请");
        private Integer key;
        private String value;

        InviteType() {
        }

        InviteType(Integer key, String value) {
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
