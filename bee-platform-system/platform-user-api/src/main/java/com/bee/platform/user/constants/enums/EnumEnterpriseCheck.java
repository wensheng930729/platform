package com.bee.platform.user.constants.enums;

/**
 * @ClassName EnterpriseCheck
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/3/19$ 10:28$
 * @version 1.0.0
 */

public class EnumEnterpriseCheck {

	public enum checkLogStatus {
		DELETED(0, "删除"), NORMAL(1, "正常");
		private Integer key;
		private String value;

		checkLogStatus() {
		}

		checkLogStatus(Integer key, String value) {
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

	public enum checkLogType {
		AUDIT_APPLY(0, "审核申请"), ADMISSION_APPLY(1, "申请入住");
		private Integer key;
		private String value;

		checkLogType() {
		}

		checkLogType(Integer key, String value) {
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

	public enum CheckType {
		REFUSED_REGISTER(0, "未通过|认证"), PASSED_REGISTER(1, "通过|认证"), IN_AUDIT_REGISTER(2, "未审核|认证"),
		REFUSED_MODIFY(3, "未通过|修改"), PASSED_MODIFY(4, "通过|修改"), IN_AUDIT_MODIFY(5, "未审核|修改");
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

		public static CheckType loanCheckType(int key) {
			for (CheckType loanCheckType : CheckType.values()) {
				if (key == loanCheckType.getKey()) {
					return loanCheckType;
				}
			}
			return null;
		}
	}

	public enum CheckTypeCommon {
		REFUSED(0, "未通过"), PASSED(1, "通过"), IN_AUDIT(2, "未审核");
		private Integer key;
		private String value;

		CheckTypeCommon() {
		}

		CheckTypeCommon(Integer key, String value) {
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

		public static CheckType loanCheckType(int key) {
			for (CheckType loanCheckType : CheckType.values()) {
				if (key == loanCheckType.getKey()) {
					return loanCheckType;
				}
			}
			return null;
		}
	}

	public enum CheckMsgType {
		REFUSED(0, "拒绝"), PASSED(1, "通过");
		private Integer key;
		private String value;

		CheckMsgType() {
		}

		CheckMsgType(Integer key, String value) {
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

		public static CheckType loanCheckType(int key) {
			for (CheckType loanCheckType : CheckType.values()) {
				if (key == loanCheckType.getKey()) {
					return loanCheckType;
				}
			}
			return null;
		}
	}

	public enum checkCompany {
		NON_EXIST(0, "没有该企业"), EXIST(1, "已有该企业"), IN_AUDIT(2, "企业审核中");
		private Integer key;
		private String value;

		checkCompany() {
		}

		checkCompany(Integer key, String value) {
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

	public enum checkRelationCompany {
		ENTERPRISE_NON_EXIST(0,"企业不存在"),
        ENTERPRISE_ALREADY_ASSOCIATED(1,"企业已关联"),
        ENTERPRISE_ASSOCIATED_IN_AUDIT(2,"企业关联正在审核中"),
        ENTERPRISE_IS_OK(3,"企业可关联");
        private Integer key;
        private String value;

        checkRelationCompany() {
        }

        checkRelationCompany(Integer key, String value) {
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
	
	public enum IS_EXISTED{
        YES(0,"企业名存在"),
        NO(1,"企业名不存在");

        private Integer key;
        private String value;
        IS_EXISTED() {
        }

        IS_EXISTED(Integer key, String value) {
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
