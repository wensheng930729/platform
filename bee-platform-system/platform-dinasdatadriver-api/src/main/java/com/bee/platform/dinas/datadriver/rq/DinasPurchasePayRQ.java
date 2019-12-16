package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("新建采购付款请求信息")
public class DinasPurchasePayRQ implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 6670306269909255569L;

	@ApiModelProperty("采购付款id")
    private Integer id;

    @NotNull(message = "采购付款编号不能为空")
    @ApiModelProperty("采购付款编号")
    private String code;

    @NotNull(message = "合同id不能为空")
    @ApiModelProperty("合同id")
    private Integer orderId;
    
    @NotNull(message = "供应商id不能为空")
    @ApiModelProperty("供应商id")
    private Integer customerId;

    @NotNull(message = "金额不能为空")
    @ApiModelProperty("金额")
    private BigDecimal amount;

    @NotNull(message = "付款日期不能为空")
    @ApiModelProperty("付款日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date payDate;

    @ApiModelProperty("附件地址")
	private List<DinasUrlRQ> list;

}
