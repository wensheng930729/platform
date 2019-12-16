package com.bee.platform.customer.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel(value = "销售订单详细列表返回相关的DTO")
public class ErpSaleOrderDetailDTO implements Serializable{

	 /**
	 * 
	 */
	private static final long serialVersionUID = -1531220897733165957L;
	private Integer id;
	    /**
	     * 关联的销售单id
	     */
	    private Integer orderId;
	    private Integer productId;
	    private String productName;
	    /**
	     * 产品数量
	     */
	    private BigDecimal num;
	    /**
	     * 单位
	     */
	    private String unit;
	    /**
	     * 含税单价
	     */
	    private BigDecimal taxPrice;
	    /**
	     * 税率，从码表取值
	     */
	    private String taxRate;
	    /**
	     * 税额
	     */
	    private BigDecimal taxAmount;
	    /**
	     * 含税金额
	     */
	    private BigDecimal amount;
	    
	    /**
	     * 是否删除
	     */
	    private Integer deleted;
	    /**
	     * 创建人id
	     */
	    private Integer createUser;
	    /**
	     * 创建时间
	     */
	    private Date createTime;
}
