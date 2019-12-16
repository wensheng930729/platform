package com.bee.platform.dinas.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;


@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("查询采购付款列表信息")
public class DinasPurchasePayDTO implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1334109117727770659L;

	@ApiModelProperty("采购付款id")
    private Integer id;

    @ApiModelProperty("采购付款编号")
    private String code;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("合同编码")
    private String orderCode;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("供应商id")
    private Integer customerId;

    @ApiModelProperty("供应商名字")
    private String customerName;

    @ApiModelProperty("金额")
    private BigDecimal amount;

    @ApiModelProperty("付款日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date payDate;

    @ApiModelProperty("附件地址")
	private List<DinasUrlDTO> list;
}
