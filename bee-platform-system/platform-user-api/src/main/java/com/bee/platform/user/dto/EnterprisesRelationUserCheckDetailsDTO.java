package com.bee.platform.user.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class EnterprisesRelationUserCheckDetailsDTO implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = -66357462424451342L;
	/**
     * 	头像
     */
    private String head;
    /**
     * 	姓名
     */
    private String nickname;
    /**
     * 	用户电话号码
     *
     */
    private String phone;
    /**
     *  企业用户id
     */
    private Integer userId;
    /**
     * 	企业用户邮箱
     */
    private String email;
    /**
     *	 固话
     */
    private String fixtel;
    /**
     * 申请理由
     */
    private String applyReason;
    /**
     * 	企业详细地址
     */
    private String enterpriseAddress;
   /**
    * 审核记录
    */
    private List list;
}
