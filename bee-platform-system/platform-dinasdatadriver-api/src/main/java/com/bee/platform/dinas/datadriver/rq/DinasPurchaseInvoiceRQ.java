package com.bee.platform.dinas.datadriver.rq;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;


@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("添加采购发票传的数据")
public class DinasPurchaseInvoiceRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = -1805185263411861187L;

	@ApiModelProperty("采购发票id")
	private Integer id;
	
	@ApiModelProperty("采购发票编号")
	private String code;
	
	@ApiModelProperty("合同编号")
	private String orderCode;
	
	@NotNull(message = "合同编号id不能为空")
	@ApiModelProperty("合同编号id")
	private Integer orderId;
	
	@ApiModelProperty("公司id")
	private Integer companyId;
	
	@ApiModelProperty("公司名称")
	private String companyName;
	
	@ApiModelProperty("开票日期")
	private String invoiceDate;
	
	@NotNull(message = "供应商id不能为空")
	@ApiModelProperty("供应商id")
	private Integer customerId;
	
	@NotNull(message = "产品id不能为空")
	@ApiModelProperty("产品id")
	private Integer productId;
	
	@NotNull(message = "产品规格id不能为空")
	@ApiModelProperty("产品规格id")
	private Integer productSpecId;
	
	@NotNull(message = "开票数量不能为空")
	@ApiModelProperty("开票数量")
	private BigDecimal num;
	
	@NotNull(message = "开票金额不能为空")
	@ApiModelProperty("开票金额")
	private BigDecimal amount;

	@ApiModelProperty("附件地址")
	private List<DinasUrlRQ> list;
}
