package com.bee.platform.user.authority.enums;

/**
 * @author liliang
 * @date 2019-5-23
 * @ClassName EnumFileType
 * @Description 附件类型（0营业执照 1营业许可证 2企业认证授权书 3logo）
 */

public class EnumFileType {
    public enum FileType{
        businessLicence(0,"营业执照"),
        openLicence(1,"开户许可证"),
        enterpriseAuthorization (2,"企业授权书"),
        logo(3,"logo"),
        ;

        FileType() {
        }

        FileType(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        private Integer key;
        private String value;

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
