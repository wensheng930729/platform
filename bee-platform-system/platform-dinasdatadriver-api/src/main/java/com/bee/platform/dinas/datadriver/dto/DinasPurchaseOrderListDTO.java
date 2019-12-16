package com.bee.platform.dinas.datadriver.dto;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 采购合同
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购合同列表信息")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class DinasPurchaseOrderListDTO implements Serializable {

    private static final long serialVersionUID = 5972169474590924660L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("合同编号")
    private String code;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("合同日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDate;

    @ApiModelProperty("供应商id")
    private Integer customerId;

    @ApiModelProperty("供应商名称")
    private String customerName;

    @ApiModelProperty("付款金额")
    private BigDecimal payment;

    @ApiModelProperty("可用金额")
    private BigDecimal availableAmount;

}
