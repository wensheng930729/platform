package com.bee.platform.customer.rq;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "客户相关")
public class AuthCustomerRQ implements Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = -202929218121788369L;
	/**
     * 企业ID
     */
    @NotNull
    private Integer enterpriseId;
    /**
     * 客户编码
     */
    private String cusNo;
    /**
     * 客户姓名
     */
    @NotNull
    private String cusName;
    /**
     * 客户一级分类
     */
    private String cusFirstType;
    /**
     * 客户二级分类
     */
    private String cusSecondType;
    /**
     * 用户状态：1启用 2禁用
     */
    private Integer status;

}
