package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 采购调价主表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购调价主表请求信息")
public class DinasPurchaseAdjustRQ implements Serializable {

    private static final long serialVersionUID = 5010076314659622234L;

    @ApiModelProperty("合同id")
    @NotNull(message = "产品id不能为空")
    private Integer orderId;

    @ApiModelProperty("调价附件")
    private List<DinasUrlRQ> url;

    @ApiModelProperty("调价日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date adjustDate;

    @ApiModelProperty("调价明细")
    @Valid
    private List<DinasPurchaseAdjustDetailRQ> adjustDetails;

}
