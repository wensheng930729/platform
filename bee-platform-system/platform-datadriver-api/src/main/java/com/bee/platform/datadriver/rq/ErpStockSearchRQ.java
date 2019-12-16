package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("现存明细请求参数")
public class ErpStockSearchRQ {
    /**
     * 企业id
     */
    @ApiModelProperty("企业id")
    private Integer orgId;

//    @ApiModelProperty("公司名称")
//    private String companyName;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;

//    @ApiModelProperty("产品名称")
//    private String productId;
    /**
     * 产品类别
     */
    @ApiModelProperty("产品类别")
    private Integer productCategory;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    private String repositoryName;

//    @ApiModelProperty("仓库名称")
//    private String repositoryId;

    private List<Integer> list;
}
