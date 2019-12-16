package com.bee.platform.datadriver.rq;

import com.bee.platform.datadriver.dto.ErpProductCategoryCheckItemsDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 产品类别
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("erp产品分类添加入参")
public class ErpProductCategoryAddRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("产品分类编码")
    private String code;

    @ApiModelProperty("产品分类名称")
    private String name;

    @ApiModelProperty("产品分类检测属性")
    private List<ErpProductCategoryCheckItemsDTO> checkItems;

    @ApiModelProperty("产品分类状态:1-启用,0-禁用")
    private Integer status;

}
