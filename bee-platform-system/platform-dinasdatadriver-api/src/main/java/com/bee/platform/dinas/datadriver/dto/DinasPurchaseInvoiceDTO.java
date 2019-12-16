package com.bee.platform.dinas.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("查询采购发票信息")
public class DinasPurchaseInvoiceDTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -8590939543336022790L;

	@ApiModelProperty("采购发票id")
	private Integer id;
	
	@ApiModelProperty("采购发票编号")
	private String code;
	
	@ApiModelProperty("合同编号")
	private String orderCode;
	
	@ApiModelProperty("合同编号id")
	private Integer orderId;
	
	@ApiModelProperty("公司id")
	private Integer companyId;
	
	@ApiModelProperty("公司名称")
	private String companyName;
	
	@ApiModelProperty("开票日期")
	@JsonFormat(pattern = "yyyy-MM-dd")
	private Date invoiceDate;
	
	@ApiModelProperty("供应商id")
	private Integer customerId;
	
	@ApiModelProperty("供应商名称")
	private String customerName;
	
	@ApiModelProperty("产品id")
	private Integer productId;
	
	@ApiModelProperty("产品名称")
	private String productName;
	
	@ApiModelProperty("产品规格id")
	private Integer productSpecId;
	
	@ApiModelProperty("产品规格名称")
	private String specName;
	
	@ApiModelProperty("开票数量")
	private BigDecimal num;
	
	@ApiModelProperty("开票金额")
	private BigDecimal amount;
	
	@ApiModelProperty("附件地址")
	private List<DinasUrlDTO> list;
	
	@ApiModelProperty("备注")
	private String remark;
}
