package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("查询采购付款列表传的条件")
public class DinasPurchasePayQueryRQ implements Serializable {


    /**
	 * 
	 */
	private static final long serialVersionUID = -6998270166386497021L;

	@ApiModelProperty("合同编码")
    private String orderCode;

    @ApiModelProperty("采购付款编号")
    private String code;

    @ApiModelProperty("供应商名字")
    private String customerName;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("付款开始日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startTime;

    @ApiModelProperty("付款结束日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endTime;
}
