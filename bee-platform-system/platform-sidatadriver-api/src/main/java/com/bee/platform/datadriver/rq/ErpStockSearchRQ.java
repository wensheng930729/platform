package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("现存明细请求参数")
public class ErpStockSearchRQ implements Serializable {
    private static final long serialVersionUID = 1L;
    /**
     * 企业id
     */
    @ApiModelProperty("企业id")
    private Integer companyId;


    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;


    /**
     * 产品类别
     */
    @ApiModelProperty("产品类别")
    private Integer productCategoryId;

//    /**
//     * 仓库名称
//     */
//    @ApiModelProperty("仓库名称")
//    private String repositoryName;


    private List<Integer> list;
}
