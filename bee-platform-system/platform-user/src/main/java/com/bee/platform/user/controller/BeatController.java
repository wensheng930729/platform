package com.bee.platform.user.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.condition.PatternsRequestCondition;
import org.springframework.web.servlet.mvc.condition.RequestMethodsRequestCondition;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import com.bee.platform.business.service.feign.BusinessBeatFeignClient;
import com.bee.platform.common.entity.Config;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.SystemCode;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.SystemCodeService;
import com.bee.platform.user.entity.User;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

/**
 * @ClassName BeatController
 * @Description 用于测试的Controller
 * @author zhigang.zhou
 * @Date 2018年11月27日 下午1:16:35
 * @version 1.0.0
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "/beat", tags = "platform-user模块测试接口")
public class BeatController {

	@Autowired
    private RedisTemplate<Object, Object> redisTemplate;

	@Autowired
    private ConfigService  configService;

	@Autowired
	private SystemCodeService systemCodeService;

	@Autowired
	private BusinessBeatFeignClient businessBeatFeignClient;
	
	@Autowired
	private WebApplicationContext applicationContext;
 
    /**
     * @Description 检查服务是否存活
     */
    @ApiOperation(value = "检查服务是否存活",notes ="检查服务是否存活")
    @GetMapping("/beat")
    public ResponseResult<UserInfo> beat() {
        log.info("platform-user-beat被调用了sysToken={}");
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
	
	@ApiOperation(value = "获取所有服务的URL资源",notes ="获取所有服务的URL资源")
    @GetMapping("/getAllUrl")
	public List<Map<String, String>> getAllUrl() {
		RequestMappingHandlerMapping mapping = applicationContext.getBean(RequestMappingHandlerMapping.class);
		// 获取url与类和方法的对应信息
		Map<RequestMappingInfo, HandlerMethod> map = mapping.getHandlerMethods();
		
		/*List<String> urlList = new ArrayList<>();
		for (RequestMappingInfo info : map.keySet()) {
			// 获取url的Set集合，一个方法可能对应多个url
			Set<String> patterns = info.getPatternsCondition().getPatterns();

			for (String url : patterns) {
				urlList.add(url);
			}
		}*/
 
		List<Map<String, String>> list = new ArrayList<Map<String, String>>();
		for (Entry<RequestMappingInfo, HandlerMethod> m : map.entrySet()) {
			Map<String, String> map1 = new HashMap<String, String>();
			RequestMappingInfo info = m.getKey();  
            HandlerMethod method = m.getValue();  
            PatternsRequestCondition p = info.getPatternsCondition();  
            for (String url : p.getPatterns()) {  
            	map1.put("url", url);
            }  
            map1.put("className", method.getMethod().getDeclaringClass().getName()); // 类名  
            map1.put("method", method.getMethod().getName()); // 方法名 
            RequestMethodsRequestCondition methodsCondition = info.getMethodsCondition();
            for (RequestMethod requestMethod : methodsCondition.getMethods()) {
            	map1.put("type", requestMethod.toString());
			}
			
            list.add(map1);
		}
		return list;
	}
    
    @ApiOperation(value = "测试用Feign客户端调用platform-business服务",notes ="测试用Feign客户端")
    @GetMapping("/businessBeatFeignClient")
    public ResponseResult<String> inputBeatFeignBeat() {
        log.info("inputBeatFeignClientBeat调platform-business-beat模块开始");
        return businessBeatFeignClient.beat();
    }
    
    
    /**
     * @Description 测试数据库

    @ApiOperation(value="获取用户列表",notes="获取全部用户信息")
    @GetMapping(value = "/selectUserList")
    public ResponseResult<List<User>> selectUserList() {
        List<User> list = userService.selectUserList();
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }*/
    
    /**
     * @Description 测试RedisTemplate
     */
    @ApiOperation(value = "测试RedisTemplate",notes ="测试RedisTemplate")
    @GetMapping("/test-redisTemplate")
    public ResponseResult<String> rediscluster() {
    	redisTemplate.opsForValue().set("redisTemplate-key-test", "hello RedisTemplate");
        String res = (String) redisTemplate.opsForValue().get("redisTemplate-key-test");
        log.debug("测试RedisCluster,res={}", res);
        String resStr = "res=" + res;
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resStr);
    }
    
	/**
	 * @Description 测试公共配置
	 */
	@ApiOperation(value = "根据键获取对应的配置", notes = "获取配置")
	@GetMapping(value = "/getConfigByconfigKey")
	public ResponseResult<Config> getConfigByconfigKey(String configKey) {
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, configService.getConfigByconfigKey(configKey));
	}
	
	/**
	 * @Description 测试公共码表信息的查询
	 */
	@ApiOperation(value = "通过缓存的键和码表组id获取码表信息", notes = "获取码表的信息")
	@GetMapping(value = "/getCacheSysCodeInfo")
	public ResponseResult<List<SystemCode>> getCacheSysCodeInfo(String redisKey, String codeTypeId) {
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
				systemCodeService.getCacheSysCodeInfo(redisKey, codeTypeId));
	}

    /**
     * @Description 测试Swagger2
     */
    @ApiOperation(value = "添加用户信息", notes = "添加用户信息")
    @ApiImplicitParam(name = "user", value = "User", required = true, dataType = "User")
    @PostMapping("/addUser")
    public ResponseResult<User> addUser(@RequestBody User user) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, user);
    }

}
