package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpStockSearchRQ
 * @Description 功能描述
 * @Date 2019/6/3 16:38
 **/


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("现存明细详情查询请求参数")
public class ErpStockDetailListRQ implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("企业id")
    @NotNull(message = "企业id不能为空")
    private Integer companyId;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("仓库名称")
    private String repositoryName;


}
