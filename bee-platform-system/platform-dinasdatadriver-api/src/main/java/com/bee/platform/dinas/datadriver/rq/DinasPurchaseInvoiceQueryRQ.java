package com.bee.platform.dinas.datadriver.rq;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("查询列表采购发票传的数据")
public class DinasPurchaseInvoiceQueryRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 8456818264141093280L;

	
	@ApiModelProperty("合同编码")
    private String orderCode;
	
	@ApiModelProperty("供应商名字")
    private String customerName;
	
	@ApiModelProperty("产品名称")
	private String productName;
	
	@ApiModelProperty("产品规格名称")
	private String productSpecName;

    @ApiModelProperty("发票开始日期")
	@JsonFormat(pattern = "yyyy-MM-dd")
    private Date startTime;

    @ApiModelProperty("发票结束日期")
	@JsonFormat(pattern = "yyyy-MM-dd")
    private Date endTime;

	@ApiModelProperty("公司id")
	private Integer companyId;
}
