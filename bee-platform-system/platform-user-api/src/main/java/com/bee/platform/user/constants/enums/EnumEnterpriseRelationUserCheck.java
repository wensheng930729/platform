package com.bee.platform.user.constants.enums;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName EnumEnterpriseRelationUserCheck
 * @Description 功能描述
 * @Date 2019/5/8 17:09
 **/
public class EnumEnterpriseRelationUserCheck {

    public enum CheckType {
        REFUSED(0, "未通过"),
        PASSED(1, "通过"),
        IN_AUDIT(2, "未审核");
        private Integer key;
        private String value;

        CheckType() {
        }

        CheckType(Integer key, String value) {
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

    public enum CheckLogType{
        AUDIT_APPLY(0,"审核申请"),
        RELATION_APPLY(1,"申请关联");
        private Integer key;
        private String value;

        CheckLogType() {
        }

        CheckLogType(Integer key, String value) {
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
