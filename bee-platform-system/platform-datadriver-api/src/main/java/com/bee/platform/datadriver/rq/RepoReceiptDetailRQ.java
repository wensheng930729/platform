package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @Classname RepoReceiptDetail
 * @Description 入库明细
 * @Date 2019/5/28 16:50
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("库存明细")
public class RepoReceiptDetailRQ implements Serializable {

    private static final long serialVersionUID = 4181264684522476304L;

    @ApiModelProperty("库存明细id")
    private Integer id;

    @ApiModelProperty("关联的仓库单据id")
    @NotNull(message = "关联的仓库单据id不能为空")
    private Integer receiptId;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("记录日期")
    private String recordTime;

    @ApiModelProperty("提单号")
    private String voucherNo;

    @ApiModelProperty("湿重")
    private BigDecimal wetWeight;

    @ApiModelProperty("计量单位")
    @NotEmpty(message = "计量单位不能为空")
    private String unit;

    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id不能为空")
    private Integer repositoryId;

    @ApiModelProperty("化验单id")
    private Integer testId;

    @ApiModelProperty("水分率")
    private String waterRate;

    @ApiModelProperty("车牌号")
    private String plateNo;

    @ApiModelProperty("毛重")
    private BigDecimal roughWeight;

    @ApiModelProperty("皮重")
    private BigDecimal weight;

    @ApiModelProperty("品位")
    private BigDecimal grade;

    @ApiModelProperty("数量")
    private BigDecimal num;

    @ApiModelProperty("公司id")
    private Integer companyId;

}
